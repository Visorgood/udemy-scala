package playground

object Implicits extends App {

  case class Person(name: String, age: Int)

  implicit val personOrder: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )

//  println(persons.sorted.map(_.name))

  case class User(name: String, email: String)

  trait Equal[T] {
    def eq(a: T, b: T): Boolean
  }

  implicit object NameEquality extends Equal[User] {
    override def eq(a: User, b: User): Boolean = a.name == b.name
  }

  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean = equalizer.eq(a, b)
  }

  implicit class EqualityEnricher[T](x: T) {
    def ===(otherX: T)(implicit equalizer: Equal[T]): Boolean = equalizer.eq(x, otherX)
    def !==(otherX: T)(implicit equalizer: Equal[T]): Boolean = !equalizer.eq(x, otherX)
  }

  val john1 = User("John", "john@gmail.com")
  val john2 = User("John", "johnny@gmail.com")
  println(Equal(john1, john2))
  println(john1 === john2)
  println(john1 !== john2)

  implicit class RichString(s: String) {
    def asInt: Int = s.toInt
    def encrypt(n: Int): String = s.map(c => (c.toInt + n).toChar)
  }

  val s1 = "Hello World!"
  val s2 = "123"
  println(s2.asInt)
  println(s1.encrypt(1))

  implicit class RichInt(x: Int) {
    def isEven: Boolean = x % 2 == 0
    def times(f:() => Unit): Unit = (1 to x).foreach(_ => f())
    def *[T](list: List[T]): List[T] = (1 to x).flatMap(_ => list).toList
  }

  3.times(() => println("Scala rocks!"))
  println(3 * List(1, 2, 3))
}
