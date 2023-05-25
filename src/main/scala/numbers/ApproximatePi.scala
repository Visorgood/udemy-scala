package numbers

import scala.util.Random

object ApproximatePi extends App {

  // compute Pi using Monte-Carlo algorithm
  def approximatePi(nPoints: Int): Double = {
    val rand = new Random()
    val nPointsInCircle =
      (1 to nPoints)
        .map(_ => {
          val x = rand.nextDouble()
          val y = rand.nextDouble()
          x * x + y * y
        })
        .count(_ < 1)
    val pi = 4.0 * nPointsInCircle / nPoints
    pi
  }

  println(s"Real Pi: ${math.Pi}")
  println(approximatePi(10))
  println(approximatePi(100))
  println(approximatePi(1000))
  println(approximatePi(10000))
  println(approximatePi(100000))
  println(approximatePi(1000000))
  println(approximatePi(10000000))
  println(approximatePi(100000000))
}
