package fr.magistry.utils.taigi

import java.io.FileWriter

import fr.magistry.nlp.scripts.Han.{TokenHan, TokenHanSeq}
import fr.magistry.nlp.tokenization.{Token, TokenNum}

import scala.io.Source

/**
  * Created by pierre on 9/10/16.
  */
object Aligner {
  val poj = new POJ(JavaNormalizer)
  val tokenizer = new TgTokenizer(poj)

  /**
    * assume NFD normalised input
    *
    * @param latin
    * @param hanji
    * @param ime
    * @return
    */
  def alignUngian(latin:String, hanji: String): List[(String, Option[String])] = {
    val tokLatin = tokenizer.linearize(tokenizer.tokenize(latin))
    val tokhj = tokenizer.linearize(tokenizer.tokenize(hanji))
      .collect({
        case TokenHanSeq(_, seq) => seq
        case TokenTaigiWord(_, seq)=> seq
        case t: TokenHan=> List(t)
        case t: TokenTaigiSyl=> List(t)
        case t: TokenNum => List(t)
      }).flatten

    def read(lt:List[Token], hj:List[Token], buf: List[(String,Option[String])]): List[(String,Option[String])] = {
      hj match {
        case Nil =>
          ((lt.mkString(" "), None) :: buf).reverse
        case _ =>
          lt match {
            case Nil => // EOL
              (hj.map(tok => ("???",Some(tok.form))) ::: buf).reverse
            case TokenTaigiWord(form, seq) :: tail =>
              read(tail, hj.drop(seq.length), (form, Some(hj.take(seq.length).map(_.form).mkString("")))::buf)
            case TokenNum(form) :: tail =>
              read(tail, hj.tail, (form, Some(hj.head.form))::buf)
            case tok :: tail =>
              read(tail, hj, (tok.form, None) :: buf)
          }
      }
    }
    read(tokLatin.toList, tokhj.toList, Nil)
  }

  def alignFile(path: String, outputPath: String): Unit = {
    val f = Source.fromFile(path)
    val fw = new FileWriter(outputPath)
    for (line <- f.getLines()) {
      val Array(pojFull,hjFull) = line.trim.split("\t")
      for ((poj,hj) <- pojFull.split("\\.+").zip(hjFull.split("(。|…)+"))) {
        val tokens = alignUngian(JavaNormalizer.normalizeNFD(poj), JavaNormalizer.normalizeNFD(hj))
        for ((in, out) <- tokens) {
          if (in != " ") {
            fw.write(in)
            out match {
              case None => ()
              case Some(hj) =>
                if (hj.toLowerCase != in.toLowerCase)
                  fw.write(s"\t$hj")
            }
            fw.write("\n")
          }
        }
      }
    }
    f.close()
    fw.close()
  }

  def test() = {
    alignFile("/tmp/PetitPrince.txt", "/tmp/PetitPrince.aligned")
  }

}
