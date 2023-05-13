package playground

object ListImpl extends App {

  abstract class MyList[+A] {
    def head: A
    def tail: MyList[A]
    def isEmpty: Boolean
    def add[B >: A](x: B): MyList[B]
    def printElements: String
    override def toString: String = "[" + printElements + "]"
    def map[B](mt: A => B): MyList[B]
    def filter(mp: A => Boolean): MyList[A]
    def flatMap[B](mt: A => MyList[B]): MyList[B]
    def ++[B >: A](list: MyList[B]): MyList[B]
    def foreach(f: A => Unit): Unit
    def sort(compare: (A, A) => Int): MyList[A]
    def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C]
    def fold[B](start: B)(operator: (B, A) => B): B
  }

  case object Empty extends MyList[Nothing] {
    def head: Nothing = throw new NoSuchElementException
    def tail: MyList[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add[B >: Nothing](x: B): MyList[B] = Cons(x, Empty)
    def printElements: String = ""
    def map[B](mt: Nothing => B): MyList[B] = Empty
    def filter(mp: Nothing => Boolean): MyList[Nothing] = Empty
    def flatMap[B](mt: Nothing => MyList[B]): MyList[B] = Empty
    def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
    def foreach(f: Nothing => Unit): Unit = ()
    def sort(compare: (Nothing, Nothing) => Int): MyList[Nothing] = Empty
    def zipWith[B, C](list: MyList[B], zip: (Nothing, B) => C): MyList[C] =
      if (!list.isEmpty) throw new RuntimeException("Lists do not have same length!")
      else Empty
    def fold[B](start: B)(operator: (B, Nothing) => B): B = start
  }

  case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
    def head: A = h
    def tail: MyList[A] = t
    def isEmpty: Boolean = false
    def add[B >: A](x: B): MyList[B] = Cons(x, this)
    def printElements: String = if (t.isEmpty) h.toString else h + " " + t.printElements
    def map[B](mt: A => B): MyList[B] = Cons(mt(h), tail.map(mt))
    def filter(mp: A => Boolean): MyList[A] = if (mp(h)) Cons(h, t.filter(mp)) else t.filter(mp)
    def flatMap[B](mt: A => MyList[B]): MyList[B] = mt(h) ++ tail.flatMap(mt)
    def ++[B >: A](list: MyList[B]): MyList[B] = Cons(h, t ++ list)
    def foreach(f: A => Unit): Unit = {
      f(h)
      t.foreach(f)
    }
    def sort(compare: (A, A) => Int): MyList[A] = {
      def insert(x: A, sortedList: MyList[A]): MyList[A] = {
        if (sortedList.isEmpty) Cons(x, Empty)
        else if (compare(x, sortedList.head) <= 0) sortedList.add(x)
        else Cons(sortedList.head, insert(x, sortedList.tail))
      }
      val sortedTail = t.sort(compare)
      insert(h, sortedTail)
    }
    def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C] = {
      if (list.isEmpty) throw new RuntimeException("Lists do not have same length!")
      else Cons(zip(h, list.head), t.zipWith(list.tail, zip))
    }
    def fold[B](start: B)(operator: (B, A) => B): B =
      t.fold(operator(start, h))(operator)
  }

  val lst1 = Cons(1, Cons(2, Cons(3, Empty)))
  println(lst1.toString)
  println(lst1.sort((x, y) => y - x).toString)
  println(lst1.fold(0)(_ + _))
  println(lst1.fold("0")(_ + " and " + _))

  val lst2 = Cons("a", Cons("b", Cons("c", Empty)))
  println(lst2.toString)
  println(lst1.zipWith(lst2, (x: Int, s: String) => s"($x and $s)").toString)
}
