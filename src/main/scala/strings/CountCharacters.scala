package strings

import scala.annotation.tailrec

object CountCharacters extends App {

  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countTailrec(remaining: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (remaining.isEmpty) acc
      else {
        val c = remaining.head
        val count = acc.getOrElse(c, 0) + 1
        val newAcc = acc + (c -> count)
        countTailrec(remaining.tail, newAcc)
      }
    }
    countTailrec(s.toLowerCase, Map.empty[Char, Int])
  }

  countCharacters("This is Scala language!").toList.sorted.foreach(println)

}
