package fr.magistry.utils.taigi

import java.io.File

import fr.magistry.nlp.LanguageModeling.{ArpaReader, BackoffLM, KenLM}
import fr.magistry.nlp.lexicon.ScoredWordList
import fr.magistry.nlp.tokenization.Token

import scala.io.Source
import scala.collection.mutable
import scala.collection.JavaConverters._
import java.sql._

import scala.Array

/**
  * Created by pierre on 9/8/16.
  */
//TODO: split in two (in RAM or in SQLite)
class ImeJVM(config: IMEConfig, arpaFile: String, val dbpath: String) extends ImeConverter(config) {
  override val poj: POJ = new POJ(JavaNormalizer)

  override val bpm: BPM = new BPM(JavaNormalizer)
  override val trs: TRS = new TRS(JavaNormalizer)
  override val LM: BackoffLM =  KenLM.loadArpa(LMOrder, arpaFile)
  override val tokenizer: TgTokenizer = new TgTokenizer(poj)

  override val AutonomyDict: ScoredWordList[Seq[Token], Double] = new ScoredWordList[Seq[Token], Double] {
    val data = mutable.HashMap.empty[String,Double]

    override def load(source: Source): Unit = {
      val LineFormat = "(.*)\t(-?[0-9]+\\.[0-9]+(?:E-[0-9]+)?)$".r
      for(line <- source.getLines().drop(2)) line match {
        case LineFormat(ng, score) =>
          data += ng -> score.toDouble
        case _ => logger.debug(s"can't match ${line}")
          throw new IllegalArgumentException()
      }
      source.close()
    }
    override def getScore(word: Seq[Token]): Option[Double] = data.get(word.map(_.form).mkString("\ue000"))
    override def contains(key: Seq[Token]): Boolean = data.contains(key.map(_.form).mkString("\ue000"))
  }

  val dbIsNew = !(new File(dbpath).exists())
  val connection: Connection = DriverManager.getConnection(s"jdbc:sqlite:$dbpath")

  def createDB(): Unit = {
    val queries = List(
        """
          | CREATE TABLE Conversions (
          |   ipa TEXT,
          |   hanlo TEXT,
          |   tailo TEXT)
        """.stripMargin,
        """
          | CREATE TABLE LmHanlo (
          |   hanlo TEXT,
          |   P FLOAT,
          |   B FLOAT)
        """.stripMargin,
        """
          | CREATE TABLE LmTailo (
          |   tailo TEXT,
          |   P FLOAT,
          |   B FLOAT)
        """.stripMargin,
        """
          | CREATE TABLE Autonomy (
          |   ngram TEXT,
          |   A FLOAT)
        """.stripMargin,
        """
          | CREATE INDEX IF NOT EXISTS ipa_idx ON Conversions(ipa)
        """.stripMargin,
        """
          | CREATE INDEX IF NOT EXISTS tailo_idx ON Conversions(tailo)
        """.stripMargin,
        """
          | CREATE INDEX IF NOT EXISTS hanlo_idx ON Conversions(hanlo)
        """.stripMargin,
        """
          | CREATE INDEX IF NOT EXISTS lmhanlo_idx ON LmHanlo(hanlo)
        """.stripMargin,
        """
          | CREATE INDEX IF NOT EXISTS autonomy_idx ON Autonomy(ngram)
        """.stripMargin)
    for( sql <- queries) {
      val stmt = connection.createStatement()
      stmt.execute(sql)
      stmt.close()
    }
  }
  if (dbIsNew)
    createDB()

  def populate(conversionsFile: String, eleveFile: String): Unit = {
    val stmtArpa = connection.prepareStatement("INSERT INTO LmHanlo(hanlo, P, B) VALUES (?,?,?)")
    val stmtConversions = connection.prepareStatement("INSERT INTO Conversions(ipa, tailo, hanlo) VALUES (?,?,?)")
    val stmtAutonomy = connection.prepareStatement("INSERT INTO Autonomy(ngram, A) VALUES(?,?)")

    connection.setAutoCommit(false)
    //LM data
    val arpaSrc = Source.fromFile(arpaFile)
    val arpaReader = new ArpaReader(arpaSrc.getLines())
    arpaReader.iterEachNgram((n, ng, P, oB) => {
      stmtArpa.setString(1, ng)
      stmtArpa.setFloat(2, P)
      oB match {
        case None => stmtArpa.setNull(3,java.sql.Types.FLOAT)
        case Some(b) => stmtArpa.setFloat(3, b)
      }
      stmtArpa.execute()
      stmtArpa.clearParameters()
    })
    arpaSrc.close()

    //El Data
    val elSrc = Source.fromFile(eleveFile)
    val LineFormat = "(.*)\t(-?[0-9]+\\.[0-9]+(?:E-[0-9]+)?)$".r
    for (line <- elSrc.getLines().drop(2)) line match {
      case LineFormat(ng, score) =>
        val s = score.toFloat
        if (s> -1.5) {
          stmtAutonomy.setString(1, ng)
          stmtAutonomy.setFloat(2, score.toFloat)
          stmtAutonomy.execute()
          stmtAutonomy.clearParameters()
        }
    }
    elSrc.close()

    //Conv data
    //TODO: more simple tsv
    def readOneCSVLine(line:String): Array[String] = {
      def aux(input: List[Char], escaped: Boolean, current: StringBuilder, buffer: List[String]): List[String] =
        if (escaped) {
          input match {
            case Nil => Nil
            case '"'::tail =>
              aux(tail, false, current , buffer)
            case l::tail => aux(tail, true, current.append(l) ,buffer)
          }
        }
        else {
          input match {
            case Nil => (current.toString() :: buffer).reverse
            case '"'::tail => aux(tail, true, current, buffer)
            case ','::tail =>
              val field = current.toString()
              current.length = 0
              aux(tail, false, current, field::buffer)
            case l::tail => aux(tail,false, current.append(l), buffer)

          }
        }
      aux(line.toList, false, new StringBuilder(), Nil).toArray
    }
    val convSrc = Source.fromFile(conversionsFile)
    convSrc.getLines().foreach( line => {
      val fields = readOneCSVLine(line)
      stmtConversions.setString(1, fields(0))
      stmtConversions.setString(2, fields(1))
      stmtConversions.setString(3, fields(2))
      stmtConversions.execute()
      stmtConversions.clearParameters()
    })
    convSrc.close()
    connection.commit()

  }

  def streamOfResultSet(resultSet: ResultSet): Stream[Int => AnyRef] = {
    if (resultSet.next())
      {key:Int => resultSet.getObject(key)} #:: streamOfResultSet(resultSet)
    else {
      resultSet.close()
      Stream.empty
    }
  }

  private val stmtHanloOfPrefix = connection.prepareStatement("SELECT hanlo FROM lmHanlo WHERE hanlo GLOB ? ORDER BY P DESC")
  override def getHanloFromPrefix(hanlo: Seq[String]): List[String] = {
    val prefixLength = (hanlo map {_.length}).foldLeft(0)(_ + _ + 1)
    stmtHanloOfPrefix.setString(1, (hanlo mkString " ") + " *")
    val resultSet = stmtHanloOfPrefix.executeQuery()
    val results = streamOfResultSet(resultSet)
      .map {_(1).asInstanceOf[String]}
      .filter { hl => !hl.contains("<\\s>") && hl.count(_ == ' ') == hanlo.length }
      .take(20)
      .toList
    stmtHanloOfPrefix.clearParameters()
    resultSet.close()
    if (results.isEmpty && hanlo.length > 1)
      getHanloFromPrefix(hanlo.tail)
    else
      results.map {_.substring(prefixLength).replace(" ", "")}
  }

  private val stmtTailoOfHanlo = connection.prepareStatement("SELECT tailo FROM Conversions WHERE hanlo = ?")
  override def getTailoOfHanlo(hanlo: String): List[String] = {
    stmtTailoOfHanlo.setString(1,hanlo)
    val results = stmtTailoOfHanlo.executeQuery()
    streamOfResultSet(results) map { _(1).asInstanceOf[String]} toList
  }

  private val stmtHanloTailoOfIpaGlob = connection.prepareStatement("SELECT hanlo, tailo FROM Conversions WHERE ipa Glob ?")
  private val stmtHanloTailoOfIpaStrict = connection.prepareStatement("SELECT hanlo, tailo FROM Conversions WHERE ipa = ?")
  override def getHanloTailoOfIpa(ipa: String, glob: Boolean): List[(String, String)] = {
    val results =
      if (glob) {
        stmtHanloTailoOfIpaGlob.setString(1, ipa)
        stmtHanloTailoOfIpaGlob.executeQuery()
      }
      else {
        stmtHanloTailoOfIpaStrict.setString(1, ipa)
        stmtHanloTailoOfIpaStrict.executeQuery()
      }
    streamOfResultSet(results) map { row => (row(1).asInstanceOf[String], row(2).asInstanceOf[String])} toList
  }


}
