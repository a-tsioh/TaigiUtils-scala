package fr.magistry.utils.taigi

import fr.magistry.nlp.scripts.Romanization
import fr.magistry.nlp.scripts.Han
import fr.magistry.nlp.tokenization._


/**
  * Created by pierre on 8/17/16.
  */
class TgTokenizer(romanization: Romanization) extends Tokenizer {


  override val grammar = Grammar(
    Set(
      BasicRule(Han.matchHanji),
      rLatin,
      rPunct,
      rNum,
      rChars) ++ romanization.tokenizationGrammar.baseRules,
    romanization.tokenizationGrammar.rules ++ Seq(Rule(Han.addHanSeq))
  )

  override def linearize(tokenDAG: TokenDAG) = linearizeWith(tokenDAG, (a,b) => {
    if (a.endAt != b.endAt) // prefer longuest match
      a.endAt > b.endAt
    else {
      (a.token, b.token) match { // prefer Words and dislike chars
        case (_:TokenTaigiWord, _) => true
        case (_, _:TokenTaigiWord) => false
        case (_:TokenTaigiSyl, _) => true
        case (_, _:TokenTaigiSyl) => false
        case (_:TokenNum, _) => true
        case (_, _:TokenNum) => false
        case (_:TokenChar,_) => false
        case (_, _:TokenChar) => true
        case _ => true // don't care about other cases
      }
    }
  })

}

