package numbers

import scala.annotation.tailrec

object ParseInteger extends App {

  def parseInteger(str: String): Int = {
    def dtoi(c: Char): Int = c - '0'
    @tailrec
    def parseTailrec(remaining: String, acc: Int, numberStarted: Boolean, signCharObtained: Boolean): Int = {
      if (remaining.isEmpty) acc
      else if (!numberStarted) {
        val c = remaining.head
        if (c == ' ') parseTailrec(remaining.tail, 1, numberStarted, signCharObtained)
        else if (c == '+' || c == '-') parseTailrec(remaining.tail, if (c == '-') -1 else 1, numberStarted, true)
        else if (c.isDigit) parseTailrec(remaining.tail, acc * dtoi(c), true, signCharObtained)
        else 0
      }
      else if (numberStarted) {
        val c = remaining.head
        if (c.isDigit) parseTailrec(remaining.tail, acc * 10 + (if (acc > 0) dtoi(c) else -dtoi(c)), numberStarted, signCharObtained)
        else acc
      }
      else 0
    }
    if (str.isEmpty) 0
    else parseTailrec(str, 1, false, false)
  }

  def printResult(number: String, expected: Int) = {
    val res = parseInteger(number)
    println(s"Parsed '$number' is $res, expected is $expected")
  }

  printResult("", 0)
  printResult("0", 0)
  printResult("00", 0)
  printResult("  00", 0)
  printResult("String", 0)
  printResult("1", 1)
  printResult("-1", -1)
  printResult("   Scala", 0)
  printResult("  4256", 4256)
  printResult("    -4256", -4256)
  printResult("   +4256", 4256)
  printResult("42 is bla bla 123", 42)
  printResult("  43 is bla bla 123", 43)
  printResult("    +44 is bla bla 123", 44)
  printResult("   -45 is bla bla 123", -45)
}
