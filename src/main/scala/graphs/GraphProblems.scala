package graphs

import scala.annotation.tailrec

object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  def outDegree[T](graph: Graph[T], node: T): Int = {
    graph.getOrElse(node, Set.empty).size
  }

  def inDegree[T](graph: Graph[T], node: T): Int = {
    graph.count(_._2.contains(node))
  }

  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    @tailrec
    def findPath(frontier: List[T], visited: Set[T]): Boolean = {
      if (frontier.isEmpty) false
      else if (visited.contains(frontier.head)) findPath(frontier.tail, visited)
      else if (frontier.head == end) true
      else findPath(frontier.tail ++ graph(frontier.head), visited + frontier.head)
    }
    graph.contains(start) && graph.contains(end) && findPath(List(start), Set.empty)
  }

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  println("outDegree and inDegree")
  println(outDegree(socialNetwork, "Mary"))
  println(inDegree(socialNetwork, "Mary"))

  println("isPath")
  println(isPath(socialNetwork, "Alice", "Mary"))
  println(isPath(socialNetwork, "Charlie", "Bob"))
  println(isPath(socialNetwork, "Charlie", "Alice"))
  println(isPath(socialNetwork, "Bob", "David"))
}
