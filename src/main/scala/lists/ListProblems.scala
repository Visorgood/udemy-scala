package lists

import scala.annotation.tailrec
import scala.util.Random

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
  def ++[S >: T](anotherList: RList[S]): RList[S]
  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  def rle: RList[(T, Int)]
  def duplicateEach(k: Int): RList[T]
  def rotate(k: Int): RList[T]
  def sample(k: Int): RList[T]

  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  override def reverse: RList[Nothing] = RNil
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList
  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  override def rle: RList[(Nothing, Int)] = RNil
  override def duplicateEach(k: Int): RList[Nothing] = RNil
  override def rotate(k: Int): RList[Nothing] = RNil
  override def sample(k: Int): RList[Nothing] = RNil

  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
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

  // if this has N elems, and anotherList has M elems -> complexity is O(N)
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def applyTailrec(remaining: RList[S], result: RList[S]): RList[S] = {
      if (remaining.isEmpty) result
      else applyTailrec(remaining.tail, remaining.head :: result)
    }
    applyTailrec(this.reverse, anotherList)
  }

  // O(N)
  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def applyTailrec(remaining: RList[T], acc: RList[T], currentIndex: Int): RList[T] = {
      if (remaining.isEmpty) acc.reverse
      else if (currentIndex == index) acc.reverse ++ remaining.tail
      else applyTailrec(remaining.tail, remaining.head :: acc, currentIndex + 1)
    }
    if (index < 0) this
    else applyTailrec(this, RNil, 0)
  }

  // O(N)
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def applyTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else applyTailrec(remaining.tail, f(remaining.head) :: acc)
    }
    applyTailrec(this, RNil)
  }

  // O(N + 2*M + M) = O(N + M), where N is the length of the input list and M is the length of the output list
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def applyTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else applyTailrec(remaining.tail, f(remaining.head).reverse ++ acc)
    }
    applyTailrec(this, RNil)
  }

  // O(N)
  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def applyTailrec(remaining: RList[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc.reverse
      else applyTailrec(remaining.tail, if (f(remaining.head)) remaining.head :: acc else acc)
    }
    applyTailrec(this, RNil)
  }

  // O(N)
  override def rle: RList[(T, Int)] = {
    @tailrec
    def applyTailrec(remaining: RList[T], acc: RList[(T, Int)], curElem: T, curCount: Int): RList[(T, Int)] = {
      if (remaining.isEmpty) ((curElem, curCount) :: acc).reverse
      else if (curElem == remaining.head) applyTailrec(remaining.tail, acc, curElem, curCount + 1)
      else applyTailrec(remaining.tail, (curElem, curCount) :: acc, remaining.head, 1)
    }
    applyTailrec(this.tail, RNil, this.head, 1)
  }

  //
  override def duplicateEach(k: Int): RList[T] = {
    this.flatMap(elem => RList.from(Seq.fill(k)(elem)))
  }

  // O(N + (k % N) * 2) = O(N + N * 2) = O(N)
  override def rotate(k: Int): RList[T] = {
    @tailrec
    def applyTailrec(remaining: RList[T], acc: RList[T], rotationsLeft: Int): RList[T] = {
      if (rotationsLeft == 0) remaining ++ acc.reverse
      else applyTailrec(remaining.tail, remaining.head :: acc, rotationsLeft - 1)
    }
    val rotationsCount = k % this.length
    if (rotationsCount == 0) this
    else applyTailrec(this, RNil, rotationsCount)
  }

  // O(N + N + K + K*log(K) + N) = O(N)
  override def sample(k: Int): RList[T] = {
    val N = this.length
    val allIndices = RList.from(Random.shuffle((0 until N).toList).take(k).sorted)
    @tailrec
    def applyTailrec(remaining: RList[T], acc: RList[T], currentIndex: Int, remainingIndices: RList[Int]): RList[T] = {
      if (remainingIndices.isEmpty) acc.reverse
      else if (currentIndex < remainingIndices.head) applyTailrec(remaining.tail, acc, currentIndex + 1, remainingIndices)
      else applyTailrec(remaining.tail, remaining.head :: acc, currentIndex + 1, remainingIndices.tail)
    }
    if (k <= 0) RNil
    else if (k >= N) this
    else applyTailrec(this, RNil, 0, allIndices)
  }

  // O(N*N)
  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def insert(remaining: RList[S], res: RList[S], elem: S): RList[S] = {
      if (remaining.isEmpty) res.reverse ++ (elem :: RNil)
      else if (ordering.lteq(elem, remaining.head)) res.reverse ++ (elem :: remaining)
      else insert(remaining.tail, remaining.head :: res, elem)
    }
    @tailrec
    def applyTailrec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else applyTailrec(remaining.tail, insert(acc, RNil, remaining.head))
    }
    applyTailrec(this, RNil)
  }

  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def merge(lst1: RList[S], lst2: RList[S], acc: RList[S]): RList[S] = {
      if (lst1.isEmpty && lst2.isEmpty) acc.reverse
      else if (lst1.isEmpty) acc.reverse ++ lst2
      else if (lst2.isEmpty) acc.reverse ++ lst1
      else if (ordering.lteq(lst1.head, lst2.head)) merge(lst1.tail, lst2, lst1.head :: acc)
      else merge(lst1, lst2.tail, lst2.head :: acc)
    }
    @tailrec
    def applyTailrec(remaining: RList[RList[S]], acc: RList[RList[S]]): RList[RList[S]] = {
      if (remaining.isEmpty) {
        if (acc.length == 1) acc
        else applyTailrec(acc.reverse, RNil)
      }
      else {
        if (remaining.tail.isEmpty) applyTailrec(remaining.tail, remaining.head :: acc)
        else applyTailrec(remaining.tail.tail, merge(remaining.head, remaining.tail.head, RNil) :: acc)
      }
    }
    applyTailrec(this.map(_ :: RNil), RNil).head
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
  val lst4 = 1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 4 :: 4 :: 4 :: 5 :: RNil
  println(lst4.toString)

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

  println("++")
  println(lst ++ largeList)
  println(largeList ++ lst)

  println("removeAt")
  println(lst.removeAt(-1))
  println(lst.removeAt(0))
  println(lst.removeAt(3))
  println(lst.removeAt(4))
  println(lst.removeAt(10))

  println("map")
  println(lst.map(_ * 3))
  println(lst3.map(_ + 15))

  println("filter")
  println(lst.filter(_ % 2 == 0))
  println(largeList.filter(_ % 17 == 5))

  println("flatMap")
  println(lst.flatMap(x => x*x :: x*x*x :: RNil))
  println(lst3.flatMap(x => x * 2 :: x * 3 :: x * 4 :: RNil))
  val time = System.currentTimeMillis()
  RList.from(1 to 10000).flatMap(x => x :: (2 * x) :: RNil)
  println(System.currentTimeMillis() - time)

  println("rle")
  println(lst4.rle)

  println("duplicateEach")
  println(lst.duplicateEach(3))

  println("rotate")
  println(lst.rotate(0))
  println(lst.rotate(2))
  println(lst.rotate(5))
  println(lst.rotate(8))
  println(lst.rotate(12))
  println(largeList.rotate(15))

  println("sample")
  println(lst.sample(-2))
  println(lst.sample(0))
  println(lst.sample(1))
  println(lst.sample(3))
  println(lst.sample(15))
  println(largeList.sample(10))

  println("sorted")
  val randomList = RList.from(Random.shuffle((1 to 100).toList))
  println(randomList)
  println((randomList ++ randomList).insertionSort(Ordering.Int))
  println((randomList ++ randomList).mergeSort(Ordering.Int))
}
