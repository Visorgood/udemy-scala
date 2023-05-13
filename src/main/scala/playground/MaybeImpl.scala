package playground

object MaybeImpl {

  abstract class Maybe[+A] {
    def get: A
    def isDefined: Boolean
    def filter(p: A => Boolean): Maybe[A]
    def map[B](t: A => B): Maybe[B]
    def flatMap[B](t: A => Maybe[B]): Maybe[B]
  }

  case object None extends Maybe[Nothing] {
    def get: Nothing = throw new NoSuchElementException
    def isDefined: Boolean = false
    def filter(p: Nothing => Boolean): Maybe[Nothing] = None
    def map[B](t: Nothing => B): Maybe[B] = None
    def flatMap[B](t: Nothing => Maybe[B]): Maybe[B] = None
  }

  case class Some[+A](x: A) extends Maybe[A] {
    def get: A = x
    def isDefined: Boolean = true
    def filter(p: A => Boolean): Maybe[A] = if (p(x)) this else None
    def map[B](t: A => B): Maybe[B] = Some(t(x))
    def flatMap[B](t: A => Maybe[B]): Maybe[B] = t(x)
  }
}
