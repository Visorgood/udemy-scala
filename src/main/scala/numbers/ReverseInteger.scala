package numbers

import scala.annotation.tailrec

object ReverseInteger extends App {

  def reverseInteger(number: Int): Int = {
    @tailrec
    def reverseTailrec(remaining: Int, acc: Int): Int = {
      if (remaining == 0) acc
      else {
        val newDigit = remaining % 10
        val newRemaining = remaining / 10
        val newAcc = acc * 10 + newDigit
        reverseTailrec(newRemaining, newAcc)
      }
    }
    reverseTailrec(number, 0)
  }

  def printResult(number: Int, expected: Int) = {
    val res = reverseInteger(number)
    println(s"Reversed $number is $res, expected is $expected")
  }

  printResult(0, 0)
  printResult(9, 9)
  printResult(53, 35)
  printResult(504, 405)
  printResult(540, 45)
  printResult(53678534, 43587635)

  printResult(-0, 0)
  printResult(-9, -9)
  printResult(-53, -35)
  printResult(-504, -405)
  printResult(-540, -45)
  printResult(-53678534, -43587635)
}
