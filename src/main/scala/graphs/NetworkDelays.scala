package graphs

import scala.annotation.tailrec

object NetworkDelays extends App {

  type Graph = Map[Int, Map[Int, Int]]

  def computeNetworkDelay(times: List[(Int, Int, Int)], source: Int): Int = {
    val graph = times.foldLeft[Graph](Map.empty) {
      case (g: Graph, (a: Int, b: Int, t: Int)) =>
        val oldChildren = g.getOrElse(a, Map.empty)
        val newChildren = oldChildren + (b -> t)
        g + (a -> newChildren)
    }
    @tailrec
    def compute(frontier: List[(Int, Int)], minTimes: Map[Int, Int]): Map[Int, Int] = {
      if (frontier.isEmpty) minTimes
      else {
        val node = frontier.head
        if (minTimes.contains(node._1) && minTimes(node._1) <= node._2) compute(frontier.tail, minTimes)
        else {
          val children = graph.getOrElse(node._1, Map.empty)
          val newFrontier = frontier.tail ++ children.toList.map(p => (p._1, p._2 + node._2))
          compute(newFrontier, minTimes + node)
        }
      }
    }
    if (!graph.contains(source)) -1
    else {
      val minTimes = compute(List((source, 0)), Map.empty)
      println(minTimes)
      minTimes.values.max
    }
  }

  println(computeNetworkDelay(
    times = List(
      (1, 2, 3), (1, 3, 10), (1, 4, 10),
      (2, 3, 4),
      (3, 4, 2)
    ),
    source = 1
  )) // 9
  println(computeNetworkDelay(
    times = List(
      (1, 2, 5), (1, 4, 13), (1, 9, 19),
      (2, 3, 7),
      (3, 4, 3), (3, 7, 1),
      (4, 5, 2), (4, 6, 2),
      (5, 6, 1),
      (6, 12, 10),
      (7, 6, 1), (7, 8, 2),
      (8, 9, 3),
      (10, 9, 2),
      (11, 10, 1)
    ),
    source = 1
  )) // 24
}
