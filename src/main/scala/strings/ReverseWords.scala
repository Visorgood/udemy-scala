package strings

object ReverseWords extends App {

  def reverseWords(s: String): String = s.split("\\s+").reverse.mkString(" ")

  println(reverseWords("Alice loves Scala"))
  println(reverseWords("   hello   world   "))
}
