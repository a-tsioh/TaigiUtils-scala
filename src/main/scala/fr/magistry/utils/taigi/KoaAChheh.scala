package fr.magistry.utils.taigi

import java.io.FileWriter

import scala.io.Source

/**
  * Created by pierre on 9/9/16.
  */
class KoaAChheh(path:String) {
  def load() = {
    def readCouplet(text: Iterator[String], hanji: List[String], poj:List[String], hanjiDone: Boolean=false): List[(String, String)] = {
      if(hanjiDone && poj.length == hanji.length) {
        hanji.zip(poj)
      }
      else {
        val nextLine = text.next()
        if (nextLine.trim == "")
          if(hanji.isEmpty) readCouplet(text, hanji, poj, false)
          else readCouplet(text, hanji, poj, true)
        else {
          if (hanjiDone)
            readCouplet(text, hanji, nextLine::poj, hanjiDone)
          else
            readCouplet(text, nextLine::hanji, poj, hanjiDone)
        }
      }
    }
    def readAll(text:Iterator[String], buf: List[(String, String)]=Nil): List[(String,String)] = {
      if (text.hasNext)
        readAll(text, readCouplet(text, Nil,Nil) ::: buf)
      else
        buf.reverse
    }
    val src = Source.fromFile(path)
    val lines = src.getLines()
    val d = readAll(lines)
    src.close()
    d
  }
  val alignedText = load()



  def alignOne(hanji:String, poj: String): String = {
    //assume last char is punctuation
    val hjW = hanji.substring(0, hanji.length - 1).split(" ")
    val pojW = poj.substring(0, poj.length - 1).split(" ")
    (for ((w1, w2) <- hjW.zip(pojW))
      yield s"$w2\t$w1").mkString("\n") +
      s"\n${hanji.substring(hanji.length -1)}\n"
  }

  def writeCorpus(path: String) = {
    val f = new FileWriter(path)
    for ((hanji, poj) <- alignedText) {
      f.write(alignOne(hanji, poj))
    }
    f.close()
  }
}
