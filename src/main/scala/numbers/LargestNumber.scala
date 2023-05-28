package numbers

import scala.annotation.tailrec
import scala.math.abs

object LargestNumber extends App {

  def largestNumber(numbers: List[Int]): String = {
    val res = numbers.map(_.toString).sortWith((s1, s2) => (s1 + s2).compareTo(s2 + s1) >= 0).mkString
    if (res.isEmpty) "0" else res
  }

  def printResult(numbers: List[Int], expected: String) = {
    val res = largestNumber(numbers)
    println(s"Result for $numbers is $res, expected is $expected")
  }

  printResult(List(), "0")
  printResult(List(1), "1")
  printResult(List(0, 0, 0), "0")
  printResult(List(1, 2, 3), "321")
  printResult(List(10, 2), "210")
  printResult(List(3, 30, 5, 9, 34), "9534330")
  printResult(List(2020, 20, 1010, 10, 2, 22), "222202020101010")
}
