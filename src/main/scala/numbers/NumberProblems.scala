package numbers

import lists.{RList, RNil}

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

  def decompose(n: Int): RList[Int] = {
    @tailrec
    def decomposeTailrec(x: Int, devisor: Int, acc: RList[Int]): RList[Int] = {
      if (x == 1) acc.reverse
      else if (devisor * devisor > x) (x :: acc).reverse
      else if (x % devisor == 0) decomposeTailrec(x / devisor, 2, devisor :: acc)
      else decomposeTailrec(x, if (devisor == 2) 3 else devisor + 2, acc)
    }
    if (n > -2 && n < 2) abs(n) :: RNil
    else if (n <= -2) decomposeTailrec(abs(n), 2, -1 :: RNil)
    else decomposeTailrec(n, 2, RNil)
  }

  println("isPrime")
  ((0 to 30) ++ Seq(2003, 2731189, -18, -17)).foreach(n => {
    val isPrimeStr = if (isPrime(n)) "" else "NOT "
    println(s"$n is ${isPrimeStr}prime")
  })

  println("decompose")
  Seq(0, 1, 2, 15, 256, 3570, 2003, 2731189, 517935871, -2003, -15).foreach(n => {
    println(s"$n decomposed: ${decompose(n)}")
  })
}
