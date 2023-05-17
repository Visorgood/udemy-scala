package lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  override def reverse: RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {

  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
    }
    "[" + toStringTailRec(this, "") + "]"
  }

  // O(min(N, index))
  override def apply(index: Int): T = {
    @tailrec
    def applyTailrec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailrec(remaining.tail, currentIndex + 1)
    }
    if (index < 0) throw new NoSuchElementException
    else applyTailrec(this, 0)
  }

  // O(N)
  override def length: Int = {
    @tailrec
    def applyTailrec(remaining: RList[T], currentLength: Int): Int = {
      if (remaining.isEmpty) currentLength
      else applyTailrec(remaining.tail, currentLength + 1)
    }
    applyTailrec(this, 0)
  }

  // O(N)
  override def reverse: RList[T] = {
    @tailrec
    def applyTailrec(remaining: RList[T], result: RList[T]): RList[T] = {
      if (remaining.isEmpty) result
      else applyTailrec(remaining.tail, remaining.head :: result)
    }
    applyTailrec(this, RNil)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def applyTailrec(remaining: Iterable[T], result: RList[T]): RList[T] = {
      if (remaining.isEmpty) result
      else applyTailrec(remaining.tail, remaining.head :: result)
    }
    applyTailrec(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  val lst = 1 :: 2 :: 3 :: 4 :: 5 :: RNil
  val largeList = RList.from(1 to 100)
  println("List definitions")
  println(lst.toString)
  println(RNil.toString)
  println(largeList)

  val lst2 = 7 :: lst
  println(lst2.toString)
  val lst3 = 9 :: RNil
  println(lst3.toString)

  println("Access by index")
//  println(lst(-5))
  println(lst(0))
  println(largeList(78))
  println(lst(4))
//  println(lst(7))

  println("Length")
  println(lst.length)
  println(lst2.length)
  println(lst3.length)

  println("Reverse")
  println(lst.reverse)
  println(largeList.reverse)
  println(lst3.reverse)

}
