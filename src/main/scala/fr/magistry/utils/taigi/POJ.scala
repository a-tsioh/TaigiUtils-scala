package fr.magistry.utils.taigi

import fr.magistry.nlp.tokenization._

import scala.util.matching.Regex

import fr.magistry.nlp.scripts.{Romanization,Normalizer, Initial, Rime, Tone, Syl, SfxMatch}

/**
  * Created by pierre on 10/04/16.
  */

case class TokenTaigiSyl(f: String) extends Token(f)

case class TokenTaigiWord(f: String, tokens: List[TokenTaigiSyl]) extends Token(f)

class POJ(normalizer: Normalizer) extends Romanization {



  val re_sfx_syllable =
    """(?i)( |-?-?)
      |(Ø|t|s|s|h|b|ch|chh|g|j|j|k|kh|l|m(?=[aeioun])|n|ng|p|ph|th|)
      |((?:[aeiou\u0358\u0300\u0301\u0302\u0304\u030d͘͘]+|n[\u0300\u0301\u0302\u0304\u030d͘͘]?g|m[\u0300\u0301\u0302\u0304\u030d͘͘]?)(?:nn|ⁿ)?)
      |(m|ng|n|p|t|h|k|)
      |([1-9]|)
      |$""".stripMargin.split("\n").mkString("").r("sep", "onset", "nucleus", "coda", "tone")

  val re_syllable =
    """(?i)(?:(?<!\w)|(-?-?))
      |(Ø|t|s|s|h|b|ch|chh|g|j|j|k|kh|l|m(?=[aeioun])|n|ng|p|ph|th|)
      |((?:[aeiou\u0358\u0300\u0301\u0302\u0304\u030d͘͘]+|n[\u0300\u0301\u0302\u0304\u030d͘͘]?g|m[\u0300\u0301\u0302\u0304\u030d͘͘]?)(?:nn|ⁿ)?)
      |(m|ng|n|p|t|h|k|)
      |([1-9]|)(?!\w)""".stripMargin.split("\n").mkString("").r("sep", "onset", "nucleus", "coda", "tone")


  val rSyls = BasicRule({ case s =>
    re_syllable.findAllMatchIn(s).map({ m => {
      Edge(m.start, m.end, TokenTaigiSyl(m.matched))
    }
    }).toSet
  })

  val rWords = Rule({case dag:TokenDAG =>
    // complete one word
    def buildWord(buffer: List[TokenTaigiSyl], position:Int): (List[TokenTaigiSyl], Int) = {
      dag.getEdgesAfter(position).flatMap({
        case Edge(_, end, t:TokenTaigiSyl) =>
          if (t.form.startsWith("-"))
            Some((t, end))
          else
            None
        case _ => None
      }).headOption match {
        case Some((tok, end)) => buildWord(tok:: buffer, end)
        case None => (buffer.reverse, position)
      }
    }

    // find all word begining and complete them
    dag.edges.flatMap({
      case Edge(begin,end,t:TokenTaigiSyl) =>
        if (!t.form.startsWith("-")) {
          val (toklist, wordEnd) = buildWord(List(t), end)
          val form = toklist.map(_.form).mkString("")
          Some(Edge(begin, wordEnd, TokenTaigiWord(form, toklist)))
        }
        else
          None
      case _ => None}) ++ dag.edges
  })

  override val tokenizationGrammar = Grammar(Set(rSyls), Seq(rWords))

  val toneMap = Map(
    2 -> "\u0301", // é
    3 -> "\u0300", // è
    5 -> "\u0302", // ê
    6 -> "\u0301", // = 2
    7 -> "\u0304", // ē
    8 -> "\u030d", //
    9 -> "" // TODO: find a proper diacritic
  )

  val revertedToneMap = Map(
    "\u0301" -> "2", // é
    "\u0300" -> "3", // è
    "\u0302" -> "5", // ê
    "\u0304" -> "7", // ē
    "\u030d" -> "8" //
  )

  def checkDottedO(rime: String): String = {
    rime match {
      case "oo" => "o\u0358"
      case "ou" => "o\u0358"
      case "Oo" => "O\u0358"
      case "Ou" => "o\u0358"
      case _ => rime
    }
  }

  def tonalise(rime: String, tone: Int): String = {
    normalizer.normalizeNFC(tone match {
      case 1 | 4 => rime
      case _ =>
        val firstChar = rime.charAt(0)
        if (rime.length > 1 && List('i', 'u', 'o').contains(firstChar.toLower) && rime.charAt(1) != 'n' && rime.charAt(1) != 'ⁿ' && !rime.toLowerCase.startsWith("oo"))
          rime.substring(0,2) + toneMap(tone) + rime.substring(2)
        else {
          val v = firstChar match {
            case 'i' => "i" //ı"  fait bugger le NFKC// NFC normalization seems work better with 'i' (?)
            case o => o.toString
          }
          v + toneMap(tone) + rime.substring(1)
        }
    })
  }

  def checkNn(finale: String): String = {
    finale
      .replace("nn", "ⁿ")
      .replace("N", "ⁿ")
  }

  /*
  Convert a match of re_poj_syllable to a Syl()
   */
  private def sylFromMatch(m: Regex.Match) = {
    def normalizeNucleus(nucleus: String): (String,Option[String]) = {
      val re_accent = "[\u0300\u0301\u0302\u0304\u030d]".r
      val n = nucleus.replace('\u0358', 'o')
      re_accent.findFirstMatchIn(n) match {
        case None => (n, None)
        case Some(m) =>
          val tone = revertedToneMap(m.matched)
          (n.replace(m.matched,""), Some(tone))
      }
    }
    val sep = m.group(1)
    val i = m.group(2)
    val (n,accentT) = normalizeNucleus(m.group(3))
    val f = m.group(4).replace("ⁿ", "nn")
    val t = m.group(5) match {
      case "" =>
        accentT match {
          case None =>
            f match {
              case "p" | "t" | "k" | "h" => 4
              case _ => 1
            }
          case Some(t) => t.toInt
        }
      case x => x.toInt
    }
    Syl(sep, Initial(i), Rime(n, f), Tone(t))
  }

  override def matchSuffix(s: String): Option[SfxMatch] = {
    def findNext(substr: String, acc: Int = 0, buffer: List[Syl] = Nil): Option[SfxMatch] = {
      re_sfx_syllable.findFirstMatchIn(substr) match {
        case None =>
          if (acc == 0) None
          else Some(SfxMatch(acc, buffer))
        case Some(m) =>
          val length =  m.group(0).length

          findNext(substr.substring(0, substr.length - length),acc + length, sylFromMatch(m) :: buffer)
      }
    }
    findNext(s)
  }

  override def parseSyllable(s: String): Option[Syl] = {
    re_sfx_syllable.findFirstMatchIn(s) match {
      case None => None
      case Some(m) =>
        if (m.start == 0) Some(sylFromMatch(m))
        else None
    }
  }

  override def parseWord(s: String): List[Syl] = {
    val sNorm = normalizer.normalizeNFD(s.trim)
    matchSuffix(sNorm) match {
      case None => Nil
      case Some(SfxMatch(length,parse)) =>
        if (length == sNorm.length) parse
        else Nil

    }

  }





  def toIPA(l: List[Syl]): List[Syl] = {

    def initialIPA(s:Syl): Initial = {
      val str = s match {
        case Syl(_, Initial(i), Rime(nucl, cod), _) =>
          val n = nucl.toLowerCase
          val f = cod.toLowerCase
          try {
            i.toLowerCase() match {
              case "ch" =>
                if (n(0) == 'i' || n == "e" && (cod == "ng" || cod == "k"))
                  "tɕ"
                else
                  "ts"
              case "j" =>
                if (n(0) == 'i' || n == "e" && (cod == "ng" || cod == "k"))
                  "ʑ"
                else
                  "dz"
              case "s" =>
                if (n(0) == 'i' || n == "e" && (cod == "ng" || cod == "k"))
                  "ɕ"
                else
                  "s"
              case "chh" =>
                if (n(0) == 'i' || n == "e" && (cod == "ng" || cod == "k"))
                  "tɕʰ"
                else
                  "tsʰ"
              case "kh" => "kʰ"
              case "ng" => "ŋ"
              case "g" => "ɡ"
              case "ph" => "pʰ"
              case "b" => "b"
              case "h" => "h"
              case "k" => "k"
              case "m" => "m"
              case "l" => "l"
              case "th" => "tʰ"
              case "n" => "n"
              case "p" => "p"
              case "t" => "t"
              case "" => ""
            }
          }
          catch {
            case e:MatchError => println("initial %s does not match POJ initial".format(i))
              throw e
          }
      }
      Initial(str)
    }
    def rimeIPA(s:Syl): Rime = {
      s match {
        case Syl(_, _, Rime(nucl, cod), _) =>
          val n = nucl.toLowerCase.replace("ⁿ","nn")
          val f = cod.toLowerCase
          val x =
            if (n.equals("oo") || n.equals("ou")) "ɔ"
            else {
              if (n == "e" && (f == "k" || f == "ng"))
                "i"
              else {
                if (n.length > 1 && n(0) != 'i' && !(n.contains("ng")))
                  n.replace('o', 'u')
                else
                  if (n.equals("ng"))
                    "ŋ"
                else
                  n
              }
            }
          val nucleus =
            if (f == "nn")
              x + "\u0303" // combining tilde~
            else
              x
          val coda =
            f match {
              case "nn" => ""
              case "ng" => "ŋ"
              case "h" => "ʔ"
              case x => x
            }
          Rime(normalizer.normalizeNFKC(nucleus), coda)
      }
    }

    def doOne(s: Syl): Syl = {
      Syl(s.sep, initialIPA(s),rimeIPA(s),s.t)
    }

    l map doOne
  }

  def ofIPA(l: List[Syl]): List[Syl] = {

    def initialIPA(s:Syl): Initial = {
      val str = s.i.s match {
        case "tɕ" => "ch"
        case "ts" => "ch"
        case "ʑ" => "j"
        case "dz" => "j"
        case "ɕ" => "s"
        case "tɕʰ" => "chh"
        case "tsʰ" => "chh"
        case "kʰ" => "kh"
        case "ŋ" => "ng"
        case "ɡ" => "g"
        case "pʰ" => "ph"
        case "tʰ" => "th"
        case other => other
      }
      Initial(str)
    }
    def rimeIPA(s:Syl): Rime = {
      val coda = s.r.f match {
        case "ŋ" => "ng"
        case "ʔ" => "h"
        case x => x // p,t,k,m,n
      }
      val nucleus =
        if (s.r.n == "i" && (s.r.f == "k" || s.r.f == "ŋ"))
          "e"
        else
          s.r.n
            .replace("ɔ", "ou")
            .replace("ua", "oa")
            .replace("ue", "oe")
            .replace("ŋ", "ng")
      Rime(normalizer.normalizeNFKC(nucleus), coda)
    }
    def doOne(s: Syl): Syl = {
      Syl(s.sep, initialIPA(s),rimeIPA(s),s.t)
    }

    l map doOne
  }

  override def toString(sounds: List[Syl]): String = {
    sounds.map {case Syl(sep, Initial(i),Rime(n, f),Tone(t)) => "%s%s%s%s".format(sep, i,tonalise(checkNn(checkDottedO(n)),t),f)} mkString("")
  }

  def toStringWithToneNumbers(sounds: List[Syl]): String = {
    sounds.map {case Syl(sep, Initial(i),Rime(n, f),Tone(t)) => "%s%s%s%s%d".format(sep, i,n,f,t)} mkString("")
  }

}
