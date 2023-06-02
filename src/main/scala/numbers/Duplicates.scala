package numbers

import scala.annotation.tailrec

object Duplicates extends App {

  // Problem: all numbers in the list appear exactly TWICE, except ONE: find that number

  def duplicatesBF(list: List[Int]): Int = {
    list.find(n => list.indexOf(n) == list.lastIndexOf(n)).get
  }

  def duplicatesSet(list: List[Int]): Int = {
    @tailrec
    def applyTailrec(remaining: List[Int], seenNumbers: Set[Int]): Set[Int] = {
      if (remaining.isEmpty) seenNumbers
      else {
        val newSeenNumbers =
          if (seenNumbers.contains(remaining.head)) seenNumbers - remaining.head
          else seenNumbers + remaining.head
        applyTailrec(remaining.tail, newSeenNumbers)
      }
    }
    applyTailrec(list, Set.empty[Int]).head
  }

  def duplicatesSort(list: List[Int]): Int = {
    @tailrec
    def applyTailrec(remaining: List[Int]): Int = {
      if (remaining.tail.isEmpty) remaining.head
      else if (remaining.head != remaining.tail.head) remaining.head
      else applyTailrec(remaining.tail.tail)
    }
    applyTailrec(list.sorted)
  }

  def duplicatesXOR(list: List[Int]): Int = list.foldLeft(0)(_ ^ _)

  def printResult(f: List[Int] => Int, lst: List[Int], expected: Int): Unit = {
    val res = f(lst)
    println(s"For $lst result is $res, expected is $expected")
  }

  println("duplicatesBF")
  printResult(duplicatesBF, List(1, 2, 3, 4, 5, 4, 3, 2, 1), 5)
  printResult(duplicatesBF, List(1, 2, 3, 4, 4, 3, 2), 1)
  printResult(duplicatesBF, List(1, 1, 2, 2, 3, 3, 4), 4)
  printResult(duplicatesBF, List(1, 2, 3, 1, 2, 3, 4), 4)

  println("duplicatesSet")
  printResult(duplicatesSet, List(1, 2, 3, 4, 5, 4, 3, 2, 1), 5)
  printResult(duplicatesSet, List(1, 2, 3, 4, 4, 3, 2), 1)
  printResult(duplicatesSet, List(1, 1, 2, 2, 3, 3, 4), 4)
  printResult(duplicatesSet, List(1, 2, 3, 1, 2, 3, 4), 4)

  println("duplicatesSort")
  printResult(duplicatesSort, List(1, 2, 3, 4, 5, 4, 3, 2, 1), 5)
  printResult(duplicatesSort, List(1, 2, 3, 4, 4, 3, 2), 1)
  printResult(duplicatesSort, List(1, 1, 2, 2, 3, 3, 4), 4)
  printResult(duplicatesSort, List(1, 2, 3, 1, 2, 3, 4), 4)

  println("duplicatesXOR")
  printResult(duplicatesXOR, List(1, 2, 3, 4, 5, 4, 3, 2, 1), 5)
  printResult(duplicatesXOR, List(1, 2, 3, 4, 4, 3, 2), 1)
  printResult(duplicatesXOR, List(1, 1, 2, 2, 3, 3, 4), 4)
  printResult(duplicatesXOR, List(1, 2, 3, 1, 2, 3, 4), 4)
}
