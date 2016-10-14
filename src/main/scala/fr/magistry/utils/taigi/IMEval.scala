package fr.magistry.utils.taigi

import java.io.FileWriter

import com.typesafe.scalalogging.LazyLogging
import fr.magistry.nlp.scripts.{Syl, Tone}

import scala.io.Source


/**
  * Created by pierre on 9/9/16.
  */
class IMEval(val n:List[Int], corpusPath: String) extends LazyLogging {
  abstract class CorpusToken
  case class Word(input:String, target:String) extends CorpusToken
  case object ContextBreak extends CorpusToken

  case class Result(accuracies: Array[Double], predictions: Array[Double], nbSelections: Int)
  case class Counts(good:Int, failed: Int, choice: Int)

  def loadCorpus() = {
    val src = Source.fromFile(corpusPath)
    val corpus = (for (line <- src.getLines()) yield {
      line.split("\t") match {
        case Array(single) => ContextBreak
        case Array(input, target) => Word(input, target)
      }
    }).toList
    src.close()
    corpus
  }
  val corpus = loadCorpus()



  def checkForPrefix(target:String, suggestions: Array[String]) = {
    suggestions.find(target.startsWith(_))
  }

  /**
    * Run evaluation of an IME
    * an IME is a function of an input and a context which returns an
    * ordered Array of suggestion and length of consummed input
    *
    * @param ime the IME function
    * @param k the number of suggestion to consider
    * @return evaluation Result
    */
  def eval(ime: (String,String) => Array[(String,Int)], k:Int, log: Boolean=false): Counts = {
    val loggedData = scala.collection.mutable.HashMap.empty[String,(Int,Set[String])]

    def aux(cursor: List[CorpusToken], context: String, counts: Counts): Counts = {
      cursor match {
        case Nil => //end of corpus
          counts
        case ContextBreak::tail => //empty context
          aux(tail, "", counts)
        case Word(input, target)::tail => //query IME
          val suggestions = ime(input, context).take(k)
          logger.info(
            s"""
               | inputing $input for $target given $context suggestions:
               | ${suggestions.map(_._1) mkString "\n"}
               | ----
             """.stripMargin)
          if (suggestions.map(_._1).contains(target)) {
            aux(tail, context + target, Counts(counts.good + 1, counts.failed, counts.choice + 1))
          }
          else
            suggestions.find({s => target.startsWith(s._1)}) match {
              case None => //IME failed in k-bests
                if (log) {
                 // println(s"failed for $target ($input)")
                  val k = s"$input\t$target"
                  val (oldCount, oldSet) = loggedData.getOrElse(k,(0,Set.empty[String]))
                  loggedData.update(k, (oldCount + 1,  (oldSet + context):Set[String]))
                }
                aux(tail, context + target, Counts(counts.good, counts.failed + 1, counts.choice + 1))
              case Some((pfx, length)) =>
                val remains = Word(input.substring(length), target.substring(pfx.length))
                aux(remains::tail, context + pfx, Counts(counts.good + 1, counts.failed, counts.choice + 1))
              }
      }
    }
    val result = aux(corpus, "", Counts(0,0,0))
    if(log){
      val f = new FileWriter("/workdir/TaigIME2/log",true)
      f.write(s"---$k-bests----\n")
      loggedData.iterator.toSeq.sortBy(_._2._1).foreach({case (k, v) =>  f.write(s"${v._1}\t$k\t${v._2 mkString ", "}\n")})
      f.write(s"-------\n\n")
      f.close()
    }
    result
  }

  def run(ime: (String, String) => Array[(String, Int)], log: Boolean=false): List[Float] = {
    n.map(k => {
      val r = eval(ime,k, log)
      r.good / (r.good + r.failed).toFloat
    })
  }

}
/*
object IMEvalTest {
  //val evalKAC1 = new IMEval(List(1,3,5,10), "/tmp/corpus.txt")
  //val evalKAC2 = new IMEval(List(1,3,5,10), "/tmp/corpus_corrected.txt")
  val evalPP = new IMEval(List(1,3,5,10), "/home/pierre/PetitPrince.aligned")
  val ime = new ImeJVM(3,"/workdir/TaigIME2/kenlm.arpa", "/workdir/TaigIME2/db.sqlite")
  ime.AutonomyDict.load(Source.fromFile("/workdir/TaigIME2/elevedata.txt"))
  val imeFun = (input: String, context: String, usePoj: Boolean) => ime.computeCandidateList(JavaNormalizer.normalizeNFD(input),  context: String, false, usePoj).map({c => (c.label, c.consumedLength)}).toArray
  val imeFunNoLM = (input: String, _: String, usePoj: Boolean) => ime.computeCandidateList(JavaNormalizer.normalizeNFD(input), "", false, usePoj).map({ c => (c.label, c.consumedLength)}).toArray
  val imeFunPrediction = (_: String, context: String, usePoj: Boolean) => ime.computeCandidateList("", context, true, usePoj).map({ c => (c.label, c.consumedLength)}).toArray
  val poj = new POJ(JavaNormalizer)
  val imeFunNoTone = (input: String, context: String, usePoj: Boolean) => {
    val tonelessInput = poj.toString(poj.ofIPA( poj.toIPA(poj.parseWord(input))).map(s => Syl(s.sep, s.i, s.r, Tone(1))))
    ime.computeCandidateList(JavaNormalizer.normalizeNFD(input),  context: String, true, usePoj).map({c => (c.label, c.consumedLength)}).toArray
  }
  println(s"I -> ${imeFun("I", "", true).mkString( " ")}")
  println(s"chit -> ${imeFun("chit", "", true).mkString( " ")}")
  def runAll() = {
    for ((eval,label, poj) <- List(/*(evalKAC1,"歌仔冊1", false), (evalKAC2,"歌仔冊2", false),*/ (evalPP,"Le Petit Prince",true))) {
      val predictions = eval.run(imeFunPrediction(_,_,poj))
      val old = eval.run(imeFunNoLM(_,_,poj))
      val withLM = eval.run(imeFun(_,_,poj), log=true)
      val fuzzy = eval.run(imeFunNoTone(_,_,poj), log=false)
      println(
      s"""
         |---------------------------------
         | $label ,${eval.n.mkString(",")}
         | Old (no LM), ${old.mkString(",")}
         | new, no tones (with LM), ${fuzzy.mkString(",")}
         | new, tones (with LM, ${withLM.mkString(",")}
         | prediction, ${predictions.mkString(",")}
         | ---------------------------------
       """.stripMargin)
    }
  }
}
*/