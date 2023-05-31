package numbers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object UglyNumber extends App {

  // ugly means: has only factors 2, 3 and 5
  // 1 is also ugly
  // assume only positive numbers
  @tailrec
  def uglyNumber(number: Int): Boolean = {
    if (number == 1) true
    else if (number % 2 == 0) uglyNumber(number / 2)
    else if (number % 3 == 0) uglyNumber(number / 3)
    else if (number % 5 == 0) uglyNumber(number / 5)
    else false
  }

  // O(M), where M is the output ugly number
  def nthUglyTrivial(index: Int): Int = {
    @tailrec
    def nthUglyTailrec(indexAcc: Int, lastUgly: Int, current: Int): Int = {
      if (indexAcc == index) lastUgly
      else {
        val next = current + 1
        if (uglyNumber(next)) nthUglyTailrec(indexAcc + 1, next, next)
        else nthUglyTailrec(indexAcc, lastUgly, next)
      }
    }
    nthUglyTailrec(0, 1, 1)
  }

  // O(N) time, where N is the index
  // O(N) space, because of queues
  def nthUglySmart(index: Int): Int = {
    def minOf3(x: Int, y: Int, z: Int): Int = {
      if (x <= y && x <= z) x
      else if (y <= x && y <= z) y
      else z
    }
    @tailrec
    def nthUglyTailrec(indexAcc: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int]): Int = {
      val min = minOf3(q2.head, q3.head, q5.head)
      if (indexAcc == index) min
      else {
        val newQ2 = (if (q2.head == min) q2.tail else q2).enqueue(min * 2)
        val newQ3 = (if (q3.head == min) q3.tail else q3).enqueue(min * 3)
        val newQ5 = (if (q5.head == min) q5.tail else q5).enqueue(min * 5)
        nthUglyTailrec(indexAcc + 1, newQ2, newQ3, newQ5)
      }
    }
    if (index == 1) 1
    else nthUglyTailrec(2, Queue[Int](2), Queue[Int](3), Queue[Int](5))
  }

  def printResult(number: Int, expected: Boolean) = {
    val res = uglyNumber(number)
    println(s"Number $number is $res, expected is $expected")
  }

  println("uglyNumber")
  printResult(1, true)
  printResult(2, true)
  printResult(3, true)
  printResult(4, true)
  printResult(5, true)
  printResult(6, true)
  printResult(7, false)
  printResult(8, true)
  printResult(9, true)
  printResult(10, true)
  printResult(14, false)
  printResult(39, false)
  printResult(256, true)
  printResult(1200, true)
  printResult(3840, true)
  printResult(5376, false)
  printResult(7 * 2 * 2 * 3 * 5 * 2 * 3 * 5, false)

  println("nthUgly")
  println((1 to 20).map(n => nthUglySmart(n)))
}
