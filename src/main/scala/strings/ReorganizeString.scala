package strings

import scala.annotation.tailrec

object ReorganizeString extends App {

  def reorganizeString(s: String): String = {
    @tailrec
    def reorg(rem: String, acc: List[Char]): List[Char] = {
      if (rem.isEmpty) acc.reverse
      else if (acc.isEmpty) reorg(rem.tail, rem.head :: acc)
      else if (rem.head != acc.head) reorg(rem.tail, rem.head :: acc)
      else if (rem.head != rem.last) reorg(rem.tail.dropRight(1), rem.head :: rem.last :: acc)
      else "IMPOSSIBLE".toCharArray.toList
    }
    reorg(s.sorted, Nil).mkString
  }

  println(reorganizeString("aaab"))
  println(reorganizeString("baaac"))
  println(reorganizeString("aabbaacab"))
}
