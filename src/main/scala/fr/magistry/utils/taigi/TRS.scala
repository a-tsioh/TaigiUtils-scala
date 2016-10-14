package fr.magistry.utils.taigi

import fr.magistry.nlp.scripts._

/**
  * Created by pierre on 14/04/16.
  */
class TRS(norm: Normalizer) extends POJ(norm) {
  override val re_sfx_syllable =
    """(?i)( |-?-?)
      |(Ø|w|t|s|si|h|b|ts|tsh|g|j|j|k|kh|l|m(?=[aeioun])|n|ng|p|ph|th|)
      |((?:[aeiou\u0358\u0300\u0301\u0302\u0304\u030d͘͘]+|n[\u0300\u0301\u0302\u0304\u030d͘͘]?g|m[\u0300\u0301\u0302\u0304\u030d͘͘]?)(?:nn|ⁿ)?)
      |(m|ng|n|p|t|h|k|)
      |([1-9]|)
      |$""".stripMargin.split("\n").mkString("").r("sep", "onset","nucleus","coda","tone")

  override val re_syllable =
    """(?i)(?:(?<!\w)|(-?-?))
      |(Ø|w|t|s|si|h|b|ts|tsh|g|j|j|k|kh|l|m(?=[aeioun])|n|ng|p|ph|th|)
      |((?:[aeiou\u0358\u0300\u0301\u0302\u0304\u030d͘͘]+|n[\u0300\u0301\u0302\u0304\u030d͘͘]?g|m[\u0300\u0301\u0302\u0304\u030d͘͘]?)(?:nn|ⁿ)?)
      |(m|ng|n|p|t|h|k|)
      |([1-9]|)(?!\w)""".stripMargin.split("\n").mkString("").r("sep", "onset","nucleus","coda","tone")

  override def toIPA(l: List[Syl]): List[Syl] = {

    def initialIPA(s:Syl): Initial = {
      val str = s match {
        case Syl(_, Initial(i), Rime(nucleus, coda), _) =>
          val n = nucleus.toLowerCase
          i.toLowerCase match {
            case "ts" =>
              if (n(0) == 'i')
                "tɕ"
              else
                "ts"
            case "j" =>
              if (n(0) == 'i')
                "ʑ"
              else
                "dz"
            case "s" =>
              if (n(0) == 'i')
                "ɕ"
              else
                "s"
            case "tsh" =>
              if (n(0) == 'i')
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
            case x =>
              println("initiale %s chelou".format(x))
              x
          }
      }
      Initial(str)
    }
    def rimeIPA(s:Syl): Rime = {
      s match {
        case Syl(_, _, Rime(nucl, cod), _) =>
          val n = nucl.replace("ⁿ","nn").toLowerCase()
          val f = cod.toLowerCase()

          val nucleus =
            if (n == "oo" || n == "ou") "ɔ"
            else
              if (n == "ng")
                "ŋ"
            else
              n
          val coda =
            f match {
              case "nn" => ""
              case "ng" => "ŋ"
              case "h" => "ʔ"
              case x => x.toLowerCase
            }
          Rime(norm.normalizeNFKC(nucleus), coda)
      }
    }

    def doOne(s: Syl): Syl = {
      Syl(s.sep, initialIPA(s),rimeIPA(s),s.t)
    }

    l map doOne
  }

  override def ofIPA(l: List[Syl]): List[Syl] = {

    def initialIPA(s:Syl): Initial = {
      val str = s.i.s match {
        case "tɕ" => "ts"
        case "ts" => "ts"
        case "ʑ" => "j"
        case "dz" => "j"
        case "ɕ" => "s"
        case "tɕʰ" => "tsh"
        case "tsʰ" => "tsh"
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
        s.r.n
          .replace("ɔ", "oo")
          .replace("ŋ", "ng")
      Rime(norm.normalizeNFKC(nucleus), coda)
    }
    def doOne(s: Syl): Syl = {
      Syl(s.sep, initialIPA(s),rimeIPA(s),s.t)
    }

    l map doOne
  }

  override def toString(sounds: List[Syl]): String = {
    sounds.map {case Syl(sep, Initial(i),Rime(n, f),Tone(t)) => "%s%s%s%s".format(sep, i, tonalise(n,t),f)} mkString("")
  }


}
