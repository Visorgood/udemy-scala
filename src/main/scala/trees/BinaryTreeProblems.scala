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
  def mirror: BTree[T]
  def sameShapeAs[S >: T](that: BTree[S]): Boolean
  def isSymmetrical: Boolean
  def toList: List[T]
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
  override def mirror: BTree[Nothing] = this
  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty
  override def isSymmetrical: Boolean = true
  override def toList: List[Nothing] = List.empty
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
  override def mirror: BTree[T] = {
    @tailrec
    def collect(todo: List[BTree[T]], visited: Set[BTree[T]], done: List[BTree[T]]): BTree[T] = {
      if (todo.isEmpty) done.head
      else if (todo.head.isEmpty || todo.head.isLeaf) collect(todo.tail, visited, todo.head :: done)
      else if (!visited.contains(todo.head)) collect(todo.head.left :: todo.head.right :: todo, visited + todo.head, done)
      else collect(todo.tail, visited, BNode(todo.head.value, done.head, done.tail.head) :: done.tail.tail)
    }
    collect(List(this), Set.empty, List.empty)
  }
  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = {
    @tailrec
    def compare(todo1: List[BTree[S]], todo2: List[BTree[S]]): Boolean = {
      if (todo1.isEmpty && todo2.isEmpty) true
      else if (todo1.isEmpty && todo2.nonEmpty || todo1.nonEmpty && todo2.isEmpty) false
      else {
        val h1 = todo1.head
        val h2 = todo2.head
        if (h1.isEmpty && h2.isEmpty) compare(todo1.tail, todo2.tail)
        else if (h1.isEmpty && !h2.isEmpty || !h1.isEmpty && h2.isEmpty) false
        else if (h1.isLeaf && h2.isLeaf) compare(todo1.tail, todo2.tail)
        else if (h1.isLeaf && !h2.isLeaf || !h1.isLeaf && h2.isLeaf) false
        else compare(h1.left :: h1.right :: todo1.tail, h2.left :: h2.right :: todo2.tail)
      }
    }
    compare(List(this), List(that))
  }
  override def isSymmetrical: Boolean = this.sameShapeAs(this.mirror)
  override def toList: List[T] = {
    @tailrec
    def collect(todo: List[BTree[T]], acc: List[T]): List[T] = {
      if (todo.isEmpty) acc.reverse
      else if (todo.head.isEmpty) collect(todo.tail, acc)
      else if (todo.head.isLeaf) collect(todo.tail, todo.head.value :: acc)
      else collect(todo.head.left :: todo.head.right :: todo.tail, todo.head.value :: acc)
    }
    collect(List(this), List.empty)
  }
}

object BinaryTreeProblems extends App {

  val tree =
    BNode(1,
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
  val otherTree =
    BNode(7,
      BNode(15,
        BNode(2, BEnd, BEnd),
        BNode(9,
          BEnd,
          BNode(5, BEnd, BEnd)
        )
      ),
      BNode(1,
        BNode(6, BEnd, BEnd),
        BNode(4, BEnd, BEnd)
      )
    )

  println(tree.collectLeaves.map(_.value))
  println(tree.size)
  (0 to 4).foreach(n => println(tree.collectNodes(n).map(_.value)))
  println(tree.mirror)
  println(tree.sameShapeAs(otherTree))
  println(tree.sameShapeAs(otherTree.right))
  println(tree.isSymmetrical)
  println(tree.right.isSymmetrical)
  println(tree.toList)
  println(otherTree.toList)

  def hasPathSumStackRec(tree: BTree[Int], target: Int): Boolean = {
    if (target < 0) false
    else if (tree.isLeaf) tree.value == target
    else !tree.left.isEmpty && hasPathSumStackRec(tree.left, target - tree.value) || !tree.right.isEmpty && hasPathSumStackRec(tree.right, target - tree.value)
  }
  println("hasPathSumStackRec")
  List(6, 7, 12, 15, 16).foreach(x => println(hasPathSumStackRec(tree, x)))

  def hasPathSumTailRec(tree: BTree[Int], target: Int): Boolean = {
    case class NodeWithTarget(node: BTree[Int], target: Int)
    @tailrec
    def traverse(todo: List[NodeWithTarget]): Boolean = {
      if (todo.isEmpty) false
      else {
        val n = todo.head.node
        val t = todo.head.target
        if (n.isEmpty || t < 0) traverse(todo.tail)
        else if (n.isLeaf && n.value == t) true
        else traverse(NodeWithTarget(n.left, t - n.value) :: NodeWithTarget(n.right, t - n.value) :: todo.tail)
      }
    }
    traverse(List(NodeWithTarget(tree, target)))
  }
  println("hasPathSumTailRec")
  List(6, 7, 12, 15, 16).foreach(x => println(hasPathSumTailRec(tree, x)))
}
