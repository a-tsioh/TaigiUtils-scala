package fr.magistry.utils.taigi

import fr.magistry.nlp.scripts._

/**
  * Created by pierre on 7/1/16.
  */
class BPM(norm: Normalizer) extends POJ(norm) {
  override val re_sfx_syllable =
    """
      |(?i)( |-?-?)
      |([ㄅㆠㄆㄇㄉㄊㄋㄌㄍㆣㄎㄫㄏㄐㆢㄑㄒㄗㆡㄘㄙ]|)
      |([ㄧㄨ]?[ㄚㆩㆦㆧㄜㆤㆥㄞㆮㄠㆯㆰㆱㆬㄢㄣㄤㆲㄥㆭㄧㆪㄨㆫ])
      |([ㄅㄉㄍㄎㄏㆴㆵㆶㆷ]|)
      |([0-9]|)$""".stripMargin.split("\n").mkString("").r("sep", "onset", "nucleus", "coda", "tone")

  override val re_syllable =
    """
      |(?i)(-?-?)
      |([ㄅㆠㄆㄇㄉㄊㄋㄌㄍㆣㄎㄫㄏㄐㆢㄑㄒㄗㆡㄘㄙ]?)
      |([ㄚㆩㆦㆧㄛㄜㆤㆥㄞㆮㄠㆯㆰㆱㆬㄢㄣㄤㆲㄥㆭㄧㆪㄨㆫ]+)
      |([ㄅㄉㄍㄏㆴㆵㆶㆷ]|)
      |([0-9]|)(?!\w)""".stripMargin.split("\n").mkString("").r("sep", "onset", "nucleus", "coda", "tone")

  override def toIPA(l: List[Syl]): List[Syl] = {

    def initialIPA(s:Syl): Initial = {
      val str = s match {
        case Syl(_, Initial(i), Rime(nucleus, coda), _) =>
          val n = nucleus.toLowerCase
          val f = coda.toLowerCase
          i.toLowerCase match {
            case "ㄐ" => "tɕ"
            case "ㄗ" => "ts"
            case "ㆢ" => "ʑ"
            case "ㆡ" => "dz"
            case "ㄒ" => "ɕ"
            case "ㄙ" => "s"
            case "ㄑ" => "tɕʰ"
            case "ㄘ" => "tsʰ"
            case "ㄎ" => "kʰ"
            case "ㄫ" => "ŋ"
            case "ㆣ" => "ɡ"
            case "ㄆ" => "pʰ"
            case "ㆠ" => "b"
            case "ㄏ" => "h"
            case "ㄍ" => "k"
            case "ㄇ" => "m"
            case "ㄌ" => "l"
            case "ㄊ" => "tʰ"
            case "ㄋ" => "n"
            case "ㄅ" => "p"
            case "ㄉ" => "t"
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
        case Syl(_, _, Rime(nucleus, coda), _) =>
          val (nucleusList, codaList) = nucleus.map({
            case 'ㄚ'=> ("a", "")
            case 'ㆩ' => ("ann", "")
            case 'ㆦ' => ("ɔ", "")
            case 'ㄛ' => ("ɔ", "")
            case 'ㆧ' => ("ɔnn", "")
            case 'ㄜ' => ("o", "")
            case 'ㆤ' => ("e", "")
            case 'ㄝ' => ("e", "")
            case 'ㆥ' => ("enn", "")
            case 'ㄞ' => ("ai", "")
            case 'ㆮ' => ("ainn", "")
            case 'ㄠ' => ("au", "")
            case 'ㆯ' => ("aunn", "")
            case 'ㆰ' => ("a", "m")
            case 'ㆱ' => ("o", "m")
            case 'ㆬ' => ("", "m")
            case 'ㄢ' => ("a", "n")
            case 'ㄣ' => ("", "n")
            case 'ㄤ' => ("a", "ŋ")
            case 'ㆲ' => ("o", "ŋ")
            case 'ㄥ' => ("", "ŋ")
            case 'ㆭ' => ("", "ŋ")
            case 'ㄧ' => ("i", "")
            case 'ㆪ' => ("inn", "")
            case 'ㄨ' => ("u", "")
            case 'ㆫ' => ("unn", "")
          }).unzip
          val nucleus2 = nucleusList.mkString

          val coda2 = codaList.mkString +
            coda match {
              case "ㄅ" => "p"
              case "ㄉ" => "t"
              case "ㄍ" => "k"
              case "ㄏ" => "ʔ"
              case "ㄎ" => "k"
              case "ㆴ" => "p"
              case "ㆵ" => "t"
              case "ㆶ" => "k"
              case "ㆷ" => "ʔ"
              case "" => ""
              case x =>
                println("pb avec la coda %s".format(x))
                x
            }
          if (nucleus2 == "")
            Rime(norm.normalizeNFKC(coda2), "")
          else
            Rime(norm.normalizeNFKC(nucleus2), coda2)
      }
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
