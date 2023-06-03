package strings

import scala.annotation.tailrec

object Justify extends App {

  def justify(text: String, width: Int): String = {
    @tailrec
    def pack(remWords: List[String], acc: List[String]): List[String] = {
      if (remWords.isEmpty) acc.reverse
      else {
        val word = remWords.head
        if (acc.isEmpty) pack(remWords.tail, word :: acc)
        else {
          val curLine = acc.head
          val newAcc = if (curLine.length + 1 + word.length <= width) s"$curLine $word" :: acc.tail else word :: acc
          pack(remWords.tail, newAcc)
        }
      }
    }

    // TODO 
    def addSpaces() = {}

    pack(text.split("\\s").toList, Nil).mkString("\n")
  }

  println(justify("Scala is a nice language", 10))
  println()
  println(justify("Scala is a strong statically typed high-level general-purpose programming language that supports both object-oriented programming and functional programming. Designed to be concise, many of Scala's design decisions are aimed to address criticisms of Java.", 40))
}
