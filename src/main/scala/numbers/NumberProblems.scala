package numbers

import scala.annotation.tailrec
import scala.math.abs

object NumberProblems extends App {

  // O(sqrt(n))
  def isPrime(n: Int): Boolean = {
    val absN = abs(n)
    @tailrec
    def isPrimeTailrec(current: Int): Boolean = {
      if (current * current > absN) true
      else if (absN % current == 0) false
      else isPrimeTailrec(current + 2)
    }
    if (absN == 0 || absN == 1) false
    else if (absN == 2) true
    else if (absN % 2 == 0) false
    else isPrimeTailrec(3)
  }

  println("isPrime")
  ((0 to 30) ++ Seq(2003, 2731189, -18, -17)).foreach(n => {
    val isPrimeStr = if (isPrime(n)) "" else "NOT "
    println(s"$n is ${isPrimeStr}prime")
  })
}
