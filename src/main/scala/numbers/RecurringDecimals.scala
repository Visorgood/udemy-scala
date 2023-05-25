package numbers

import scala.annotation.tailrec

object RecurringDecimals extends App {

  def toDecimal(numerator: Int, denominator: Int): String = {
    val whole = numerator / denominator
    val rest = numerator % denominator

    val ds = denominator.toString.length
    val multiplier = Seq.fill(ds)(10).product

    @tailrec
    def calcNextDecimal(rest: Int, index: Int, restMap: Map[Int, Int], acc: String): String = {
      if (rest == 0) acc
      else if (restMap.contains(rest)) {
        val indexOfFirstOccurrence = restMap(rest)
        s"${acc.substring(0, indexOfFirstOccurrence)}(${acc.substring(indexOfFirstOccurrence)})"
      }
      else {
        val mult = rest * multiplier
        val newDecimal = mult / denominator
        val newRest = mult % denominator
        val lpad = s"%0${ds}d"
        val newDecimalLPAD = lpad.format(newDecimal)
        calcNextDecimal(newRest, index + 1, restMap + (rest -> index), s"$acc$newDecimalLPAD")
      }
    }

    if (rest == 0) whole.toString
    else {
      val decimalPart = calcNextDecimal(rest, 0, Map(), "")
      s"$whole.$decimalPart"
    }
  }

  def callAndPrintToDecimal(numerator: Int, denominator: Int, expected: String) = {
    val res = toDecimal(numerator, denominator)
    println(s"$numerator/$denominator = $res, expected $expected")
  }

  callAndPrintToDecimal(1, 2, "0.5")
  callAndPrintToDecimal(9, 8, "1.125")
  callAndPrintToDecimal(4, 2, "2")
  callAndPrintToDecimal(27, 9, "3")
  callAndPrintToDecimal(1, 3, "0.(3)")
  callAndPrintToDecimal(4, 3, "1.(3)")
  callAndPrintToDecimal(1, 6, "0.1(6)")
  callAndPrintToDecimal(13, 6, "2.1(6)")
  callAndPrintToDecimal(1, 333, "0.(003)")
  callAndPrintToDecimal(1, 7, "0.(142857)")
  callAndPrintToDecimal(1, 2003, "???")
}
