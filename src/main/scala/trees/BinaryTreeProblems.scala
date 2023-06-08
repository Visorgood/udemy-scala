package trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]

  def isEmpty: Boolean
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int
  val size: Int
  def collectNodes(level: Int): List[BTree[T]]
}
case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def left: BTree[Nothing] = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true
  override def isLeaf: Boolean = false
  override def collectLeaves: List[BTree[Nothing]] = List.empty
  override def leafCount: Int = 0
  override val size: Int = 0
  override def collectNodes(level: Int): List[BTree[Nothing]] = List.empty
}
case class BNode[+T](override val value: T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T] {
  override def isEmpty: Boolean = false
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty
  override def collectLeaves: List[BTree[T]] = {
    @tailrec
    def collect(todo: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] = {
      if (todo.isEmpty) leaves
      else if (todo.head.isEmpty) collect(todo.tail, leaves)
      else if (todo.head.isLeaf) collect(todo.tail, todo.head :: leaves)
      else collect(todo.head.left :: todo.head.right :: todo.tail, leaves)
    }
    collect(List(this), List.empty)
  }
  override def leafCount: Int = collectLeaves.length
  override val size: Int = 1 + left.size + right.size
  override def collectNodes(level: Int): List[BTree[T]] = {
    case class NodeWithLevel(node: BTree[T], level: Int)
    @tailrec
    def collect(todo: List[NodeWithLevel], leaves: List[BTree[T]]): List[BTree[T]] = {
      if (todo.isEmpty) leaves
      else if (todo.head.node.isEmpty) collect(todo.tail, leaves)
      else if (todo.head.level == 0) collect(todo.tail, todo.head.node :: leaves)
      else {
        val leftNode = NodeWithLevel(todo.head.node.left, todo.head.level - 1)
        val rightNode = NodeWithLevel(todo.head.node.right, todo.head.level - 1)
        val newTodo = leftNode :: rightNode :: todo.tail
        collect(newTodo, leaves)
      }
    }
    collect(List(NodeWithLevel(this, level)), List.empty)
  }
}

object BinaryTreeProblems extends App {
  val tree = BNode(1,
    BNode(2,
      BNode(3, BEnd, BEnd),
      BNode(4,
        BEnd,
        BNode(5, BEnd, BEnd)
      )
    ),
    BNode(6,
      BNode(7, BEnd, BEnd),
      BNode(8, BEnd, BEnd)
    )
  )

  println(tree.collectLeaves.map(_.value))
  println(tree.size)
  (0 to 4).foreach(n => println(tree.collectNodes(n).map(_.value)))
}
