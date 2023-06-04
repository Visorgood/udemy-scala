package strings

import scala.annotation.tailrec

object Justify extends App {

  def justify(text: String, width: Int): String = {
    @tailrec
    def pack(remWords: List[String], acc: List[List[String]]): List[List[String]] = {
      if (remWords.isEmpty) acc.reverse
      else {
        val word = remWords.head
        if (acc.isEmpty) pack(remWords.tail, List(word) :: acc)
        else {
          val curLine = acc.head
          val curLineLength = curLine.map(_.length + 1).sum
          val newAcc =
            if (curLineLength + word.length <= width) (word :: curLine) :: acc.tail
            else List(word) :: acc
          pack(remWords.tail, newAcc)
        }
      }
    }
    def addSpaces(text: List[List[String]]): List[String] = {
      text.map { line: List[String] =>
        val totalWordsLength = line.foldLeft(0)(_ + _.length)
        val lengthForSpaces = width - totalWordsLength
        val slotsForSpaces = line.length - 1
        if (slotsForSpaces > 0) {
          val baseNumberOfSpaces = lengthForSpaces / slotsForSpaces
          val slotsWithOneMoreSpace = lengthForSpaces % slotsForSpaces
          val spaces = Seq.fill(baseNumberOfSpaces)(" ").mkString
          if (slotsWithOneMoreSpace > 0) {
            val n = slotsWithOneMoreSpace + 1
            val part1 = line.take(n).mkString(spaces + " ")
            val part2 = line.drop(n).mkString(spaces)
            part1 + spaces + part2
          }
          else line.mkString(spaces)
        }
        else line.head + Seq.fill(lengthForSpaces)(" ").mkString
      }
    }

    addSpaces(pack(text.split("\\s").toList, Nil).map(_.reverse)).mkString("\n")
  }

  val s = "Scala is a strong statically typed high-level general-purpose programming language that supports both object-oriented programming and functional programming. Designed to be concise, many of Scala's design decisions are aimed to address criticisms of Java."
  println(justify(s, 40))
}
