package strings

import scala.annotation.tailrec

object CompareVersionNumbers extends App {

  def compareVersionNumbers(version1: String, version2: String): Int = {
    def toIntList(v: String): List[Int] = v.split('.').map(_.toInt).toList
    @tailrec
    def compare(rem1: List[Int], rem2: List[Int]): Int = {
      if (rem1.isEmpty && rem2.isEmpty) 0
      else if (rem1.isEmpty) {if (rem2.sum == 0) 0 else -1}
      else if (rem2.isEmpty) {if (rem1.sum == 0) 0 else 1}
      else if (rem1.head > rem2.head) 1
      else if (rem1.head < rem2.head) -1
      else compare(rem1.tail, rem2.tail)
    }
    compare(toIntList(version1), toIntList(version2))
  }

  println(compareVersionNumbers("1.0.2.3", "1.0.02.2"))
  println(compareVersionNumbers("1.0.2.3", "1.0.02.4"))
  println(compareVersionNumbers("1.0.2", "1.0"))
  println(compareVersionNumbers("1.0.2", "1.0.2.0"))
  println(compareVersionNumbers("1.0.0", "1.0.0.0"))
  println(compareVersionNumbers("0.5.7", "1.0"))
  println(compareVersionNumbers("0.05.7.2", "0.5.7.1"))
}
