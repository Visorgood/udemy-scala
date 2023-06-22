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
    def isPathTailrec(frontier: List[T], visited: Set[T]): Boolean = {
      if (frontier.isEmpty) false
      else if (visited.contains(frontier.head)) isPathTailrec(frontier.tail, visited)
      else if (frontier.head == end) true
      else isPathTailrec(frontier.tail ++ graph(frontier.head), visited + frontier.head)
    }
    graph.contains(start) && graph.contains(end) && isPathTailrec(List(start), Set.empty)
  }

  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {
    @tailrec
    def findPathTailrec(frontier: List[T], visited: Set[T], acc: List[T]): List[T] = {
      if (frontier.isEmpty) List.empty
      else if (frontier.head == end) (end :: acc).reverse
      else if (visited.contains(frontier.head)) {
        if (acc.head == frontier.head) findPathTailrec(frontier.tail, visited, acc.tail)
        else findPathTailrec(frontier.tail, visited, acc)
      }
      else findPathTailrec(graph(frontier.head).toList ++ frontier, visited + frontier.head, frontier.head :: acc)
    }
    if (!graph.contains(start) || !graph.contains(end)) List.empty
    else findPathTailrec(List(start), Set.empty, List.empty)
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

  println("findPath")
  println(findPath(socialNetwork, "Alice", "Mary"))
  println(findPath(socialNetwork, "Charlie", "Bob"))
  println(findPath(socialNetwork, "Charlie", "Alice"))
  println(findPath(socialNetwork, "Bob", "David"))
  println(findPath(socialNetwork, "Alice", "Tom"))
  println(findPath(socialNetwork, "Tom", "Alice"))
}
