package fr.magistry.utils.taigi

import com.typesafe.scalalogging.LazyLogging
import fr.magistry.nlp.LanguageModeling.BackoffLM
import fr.magistry.nlp.lexicon.ScoredWordList
import fr.magistry.nlp.scripts.Han.{TokenHan, TokenHanSeq}
import fr.magistry.nlp.scripts.{Initial, Rime, Syl, Tone}
import fr.magistry.nlp.segmentation.Ambiguous
import fr.magistry.nlp.{segmentation, tokenization}
import fr.magistry.nlp.tokenization.Token

abstract class ImeItem
case class CompositionItem(label: String, consumedLength: Int) extends ImeItem
case class CorrectionItem(label: String, correctedString: String) extends ImeItem

sealed abstract class SegmentationConfig
case class LexiconBasedSegmentation(path: String) extends SegmentationConfig
case class EleveConfig(order: Int, nBests: Seq[tokenization.Token] => Int, keepPercentage: Double) extends SegmentationConfig
case class MixConfig(lexConfig: LexiconBasedSegmentation, eleveConfig: EleveConfig) extends SegmentationConfig

case class LMConfig(order: Int, pruning: Option[String], allowUnk:Boolean, addStart: Boolean)

case class IMEConfig(workdir: String, sc: SegmentationConfig, lm: LMConfig)

/**
  * Created by pierre on 9/7/16.
  */
abstract class ImeConverter(config: IMEConfig) extends  LazyLogging {
  val poj: POJ
  val trs: TRS
  val bpm: BPM

  val tokenizer: TgTokenizer

  val LM: BackoffLM
  val LMOrder = config.lm.order

  val AutonomyDict: ScoredWordList[Seq[tokenization.Token], Double]
  val ElOrder = config.sc match {
    case LexiconBasedSegmentation(_) => None
    case EleveConfig(order,_,_) => Some(order)
    case MixConfig(_,cfg) => Some(cfg.order)
  }

  val maxWordLength = ElOrder.getOrElse(6)

  def getHanloTailoOfIpa(ipa: String, glob: Boolean=false): List[(String,String)]
  def getTailoOfHanlo(hanlo: String): List[String]
  def getHanloFromPrefix(hanlo: Seq[String]): List[String]

  /**
    * Return a clean romanization from a input romanized with numbers or in zhuyin
    * (TODO: Zhuyin)
    *
    * @param input
    * @return Some normalized string if parsing succeeded
    */
  def normalizeRomanization(input: String, inputIsPOJ: Boolean, outputIsPOJ: Boolean): Option[String] = {
    val syls = if (inputIsPOJ)
      poj.toIPA(poj.parseWord(input))
    else
      trs.toIPA(trs.parseWord(input))
    if (syls.isEmpty)
      None
    else
      (if (outputIsPOJ)
        Some(poj.toString(poj.ofIPA(syls)))
      else
        Some(trs.toString(trs.ofIPA(syls)))
        ).map(result => {
        if (input(0).isUpper)
          result.capitalize
        else
          result
      })
  }

  /**
    * compute all possible conversions of the current composition
    *
    * @param input
    * @return
    */
  def getConversions(input: String, fuzzyMatches: Boolean, usePOJ: Boolean): Set[(String, Int)] = {
    def getSylsLength(syls:List[Syl]): Int = {
      syls.foldLeft(0)((total, s) => {
        total + s.i.s.length + s.r.n.length + s.r.f.length + s.sep.length
      })
    }

    val (syls,romanization) = bpm.parseWord(input) match {
      case Nil => // parse as zhuyin failed
        if (usePOJ)
          (poj.parseWord(input), poj)
        else
          (trs.parseWord(input), trs)
      case something => (something, bpm)
    }
    if (syls.nonEmpty) {
      val tailo = romanization.toIPA(syls)
      if (fuzzyMatches) {
        val tonelessIpa = tailo map { case Syl(sep, Initial(i), Rime(n, f), Tone(t)) => "%s.%s.%s.%s".format(i, n, f, if (t == 1 || t == 4) "?" else t.toString) }
        (tailo.length to 1 by -1).foldLeft(Set.empty[(String,Int)])((result, i) => {
          val query = tonelessIpa.slice(0,i) mkString " "
          val couples = getHanloTailoOfIpa(query, glob = true).map({case (hl, tl) => (hl, getSylsLength(syls.take(i)))})
          result ++ couples
        })
      }
      else {
        val ipa = tailo map { case Syl(sep, Initial(i), Rime(n, f), Tone(t)) => "%s.%s.%s.%d".format(i, n, f, t) }
        (tailo.length to 1 by -1).foldLeft(Set.empty[(String,Int)])((result, i) => {
          val query = ipa.slice(0, i) mkString " "
          val couples = getHanloTailoOfIpa(query).map({ case (hl, tl) => (hl, getSylsLength(syls.take(i))) })
          result ++ couples
        })
      }
    }
    else {
      if (input.length == 0)
        Set.empty
      else
        getConversions(input.substring(0,input.length -1),fuzzyMatches, usePOJ)
    }
  }

  def tokenize(input:String): Seq[Token] = {
    tokenizer.linearize(tokenizer.tokenize(input))
  }

  def generateMask(input: Array[Token]): Array[segmentation.MaskValue] ={
    val params = config.sc match {
      case EleveConfig(_, bgFun,p) => Some((bgFun(input), 1.0 - p))
      case MixConfig(_,cfg) => Some((cfg.nBests(input), 1.0 - cfg.keepPercentage))
      case _ => None
    }
    params match {
      case Some((nbests, proportion)) =>
        val segs = segmentation.maximizeScore(input, AutonomyDict, ElOrder.getOrElse(1), nbests)

        val bestScore = segs.head.score
        // fix for negative values
        val targetScore =
          if (bestScore >= 0) bestScore * proportion
          else math.log( math.exp(bestScore) * proportion)
        val minScore = math.min(bestScore, targetScore)
        val results = segs.filter(_.score >= minScore)
        logger.debug(s"segmentations for mask\n${results.map(_.cuts.mkString(", ")).mkString("\n")}\n")
        val mask = segmentation.createMask(results)
        logger.debug(mask.mkString(" "))
        mask
      case None =>
        Array.fill[segmentation.MaskValue](input.length + 1)(Ambiguous)
    }

  }

  /**
    * rank conversions candidates according to the context
    *
    * @param conversions
    * @param context
    * @param cut
    * @return
    */
  def rankCandidates(conversions: Set[(String, Int)], context: String, cut: Int=50): List[(String, Int)] = {
    conversions.map({case (candidate, size) =>
      val sentence = context + candidate
      val tokens = tokenizer.linearize(tokenizer.tokenize(sentence))
      val score = tokens.map({
        case TokenHanSeq(_, subseq) =>
          val mask = generateMask(subseq.toArray)
          val result = segmentation.applyLM(subseq.map(_.form).toArray, LM, LMOrder, maxWordLength, Some(mask), config.lm.allowUnk, config.lm.addStart)
          result.score
        case TokenHan(form) =>
          segmentation.applyLM(Array(form),LM, LMOrder, maxWordLength, None, config.lm.allowUnk, config.lm.addStart).score
        case _ => 0.0
      }).sum
      (score, candidate, size)
    }).toList.sortBy(t => (t._3,t._1)).map(t => (t._2, t._3)).reverse
  }

  def computeCandidateList(input:String, context:String, fuzzy:Boolean, usePOJ: Boolean): List[CompositionItem] = {
    val candidates: Set[(String, Int)] = input match {
      case "" =>
        if (context == "")
          Set.empty[(String, Int)]
        else {
          val toks = tokenize(context) flatMap {
            case TokenHanSeq(_, subseq) => subseq
            case tok => List(tok)
          }
          val mask = generateMask(toks.toArray)
          val seg = segmentation.applyLM(toks.map(_.form).toArray, LM, LMOrder, maxWordLength, Some(mask), config.lm.allowUnk, config.lm.addStart).words
          getHanloFromPrefix(seg).map((_,0)).toSet
        }
      case _ => getConversions(input, fuzzy, usePOJ)
    }
    rankCandidates(candidates.filter({c => c._1.exists(Character.isIdeographic(_))}), context).map({ case (c, size) => CompositionItem(normalizeRomanization(c, false, usePOJ).getOrElse(c), size) })
  }


  def computeCorrections(input: String, strict: Boolean, usePOJ: Boolean): List[CorrectionItem] = {
    if (strict)
      getTailoOfHanlo(input).map(c => CorrectionItem(normalizeRomanization(c, false, usePOJ).getOrElse(c), input))
    else {
      ((input.length - 1) to 0 by -1).foldLeft(Nil:List[CorrectionItem])((acc,start) => {
        val substr = input.substring(start)
        getTailoOfHanlo(substr).map(c => CorrectionItem(normalizeRomanization(c, false, usePOJ).getOrElse(c), substr)) ++ acc
      })
    }

  }

}
