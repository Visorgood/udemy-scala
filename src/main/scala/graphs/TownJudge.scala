package graphs

object TownJudge extends App {

  type Graph = Map[Int, Set[Int]]

  def findJudge(n: Int, trust: List[(Int, Int)]): Int = {
    val graph = trust.foldLeft[Graph](Map.empty) {
      case (g: Graph, (a: Int, b: Int)) =>
        val newSet = g.getOrElse(a, Set.empty) + b
        g + (a -> newSet)
    }
    val commonTrustees = graph.foldLeft((1 to n).toSet) {
      case (ct: Set[Int], (_: Int, trustees: Set[Int])) =>
        ct intersect trustees
    }
    if (commonTrustees.isEmpty || commonTrustees.size > 1 || graph.contains(commonTrustees.head)) -1
    else commonTrustees.head
  }

  println(findJudge(2, List((1, 2)))) // 2
  println(findJudge(3, List((1, 2), (3, 2)))) // 2
  println(findJudge(3, List((1, 3), (2, 3), (3, 3)))) // -1
  println(findJudge(3, List((1, 2), (2, 3), (3, 1)))) // -1
  println(findJudge(4, List((1, 3), (2, 3), (1, 4), (2, 4), (3, 4)))) // 4
}
