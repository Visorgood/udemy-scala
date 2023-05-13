package playground

import scala.annotation.tailrec

object Playground {
  def main(args: Array[String]): Unit = {

//    println("Factorial:")
//    (1 to 10).foreach(n => println(n + " => " + factorial(n)))
//    println()
//    println("Fibonacci:")
//    (1 to 10).foreach(n => println(n + " => " + fibonacci(n)))
//    println()
//    println("IsPrime:")
//    (1 to 30).foreach(n => println(n + (if (isPrime(n)) " is " else " is not ") + "prime"))
//    println()

//    println(concatenateString("abc", 3))

//    println(fibonacci1(10))
//    println(fibonacci2(10))

//    println(concatenator1("Hello ", "Scala1"))
//    println(concatenator1("Hello ", "Scala11"))
//    println(concatenator2("Hello ", "Scala2"))
//    println(concatenator2("Hello ", "Scala22"))

    val lst1 = List(1, 2, 3, 4)
    val lst2 = List("a", "b", "c", "d")
    println(lst1.flatMap(x => lst2.map(c => s"$c$x")))

    val x = Array(1, 2, 3)
  }

  def twoSumProblem(array: Array[Int], K: Int): Boolean = {
    val valueToCount: Map[Int, Int] = array.zipWithIndex.groupBy(_._1).map(x => (x._1, x._2.length))
    array.exists(x => {
      val y = K - x
      valueToCount.get(y) match {
        case Some(yCount) => x != y || yCount > 1
        case None => false
      }
    })
  }

  def factorial(n: Int): Long = {
    if (n <= 1) 1L else n * factorial(n - 1)
  }

  def fibonacci1(n: Int): Long = {
    if (n <= 2) 1L else fibonacci1(n - 1) + fibonacci1(n - 2)
  }

  def fibonacci2(n: Int): Long = {
    @tailrec
    def fibonacciTailrec(k: Int, prev1: Long, prev2: Long): Long = {
      if (k > n) prev1
      else fibonacciTailrec(k + 1, prev1 + prev2, prev1)
    }
    fibonacciTailrec(3, 1, 1)
  }

  def isPrime(n: Long): Boolean = {
    if (n == 2) true
    else {
      if (n <= 1 || n % 2 == 0) false
      else {
        @tailrec
        def isNotDivisibleByAnyOddFrom(k: Long): Boolean = {
          if (k * k > n) true
          else n % k != 0 && isNotDivisibleByAnyOddFrom(k + 2)
        }
        isNotDivisibleByAnyOddFrom(3)
      }
    }
  }

  def concatenateString(s: String, n: Int): String = {
    @tailrec
    def concatenateStringHelper(k: Int, acc: String): String = {
      if (k == 0) acc
      else concatenateStringHelper(k - 1, acc + s)
    }
    concatenateStringHelper(n, "")
  }

  def concatenator1: (String, String) => String = (s1: String, s2: String) => s1 + s2

  val concatenator2: (String, String) => String = (s1: String, s2: String) => s1 + s2

  def addFunc(x: Int): Int => Int = (y: Int) => x + y

  val addFunc2 = (x: Int) => (y: Int) => x + y

  def toCurry(f: (Int, Int) => Int): Int => (Int => Int) =
    (x: Int) => (y: Int) => f(x, y)

  def fromCurry(f: Int => (Int => Int)): (Int, Int) => Int =
    (x: Int, y: Int) => f(x)(y)

  def compose(f: Int => Int, g: Int => Int): Int => Int =
    (x: Int) => f(g(x))

  def andThen(f: Int => Int, g: Int => Int): Int => Int =
    (x: Int) => g(f(x))
}

class Writer(firstName: String, surName: String, val year: Int) {
  def fullname(): String = s"$firstName $surName"
}

class Novel(name: String, yearOfRelease: Int, author: Writer) {
  def authorAge(): Int = yearOfRelease - author.year
  def isWrittenBy(author: Writer): Boolean = author.fullname() == this.author.fullname() && author.year == this.author.year
  def copy(newYearOfRelease: Int): Novel = new Novel(name, newYearOfRelease, author)
}

class Counter(val count: Int = 0) {
  def inc(n: Int): Counter = new Counter(count + n)
  def dec(n: Int): Counter = inc(-n)
  def inc(): Counter = inc(1)
  def dec(): Counter = inc(-1)
}
