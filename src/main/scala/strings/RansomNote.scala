package strings

object RansomNote extends App {

  def ransomNote(note: String, magazine: String): Boolean = {
    val noteChars = CountCharacters.countCharacters(note)
    val magazineChars = CountCharacters.countCharacters(magazine)
    noteChars.forall(x => x._2 <= magazineChars.getOrElse(x._1, 0))
  }

  val s1 = "this is some text"
  val s2 = "here i have some other text for this interesting magazine"
  println(ransomNote(s1, s2))

  val s3 = "this is some text"
  val s4 = "here i have other text for this interesting journal"
  println(ransomNote(s3, s4))
}
