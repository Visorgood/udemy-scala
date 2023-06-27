package graphs

import graphs.GraphProblems.Graph

import scala.annotation.tailrec

object UniCourses extends App {

  def findOrder(n: Int, prereqs: List[(Int, Int)]): List[Int] = {
    val graph = prereqs.foldLeft[Graph[Int]](Map.empty) {
      case (g: Graph[Int], (a: Int, b: Int)) =>
        val newSet = g.getOrElse(a, Set.empty) + b
        g + (a -> newSet)
    }
    @tailrec
    def findOrderTailrec(stack: List[Int], nodesToConsider: Set[Int], visited: Map[Int, Int], res: List[Int]): List[Int] = {
      if (nodesToConsider.isEmpty) res.reverse
      else if (stack.isEmpty) findOrderTailrec(nodesToConsider.head :: stack, nodesToConsider, Map.empty, res)
      else if (!nodesToConsider.contains(stack.head)) findOrderTailrec(stack.tail, nodesToConsider, visited, res)
      else if (visited.getOrElse(stack.head, 0) >= 2) List.empty
      else {
        val children = graph.getOrElse(stack.head, Set.empty).intersect(nodesToConsider).toList
        val newVisitedCount = visited.getOrElse(stack.head, 0) + 1
        val newVisited = visited + (stack.head -> newVisitedCount)
        if (children.isEmpty) findOrderTailrec(stack.tail, nodesToConsider - stack.head, newVisited, stack.head :: res)
        else findOrderTailrec(children ++ stack, nodesToConsider, newVisited, res)
      }
    }
    findOrderTailrec(List.empty, (0 until n).toSet, Map.empty, List.empty)
  }

  println(findOrder(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4)))) // e.g. List(1, 0, 4, 5, 2, 3)

  println(findOrder(11, List(
    (0, 1), (0, 2), (0, 8),
    (1, 2),
    (2, 3), (2, 4), (2, 5),
    (3, 4),// (3, 0),
    (4, 6),
    (5, 7),
    (8, 9), (8, 10),
    (9, 3),
    (10, 9)
  ))) // e.g. List(6, 4, 3, 7, 5, 2, 1, 9, 10, 8, 0)
}
