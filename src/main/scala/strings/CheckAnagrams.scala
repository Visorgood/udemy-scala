package strings

object CheckAnagrams extends App {

  def checkAnagramsCount(sa: String, sb: String): Boolean = {
    val freqsSA = CountCharacters.countCharacters(sa)
    val freqsSB = CountCharacters.countCharacters(sb)
    freqsSA == freqsSB
  }
  
  def checkAnagramsSort(sa: String, sb: String): Boolean = {
    sa.sorted == sb.sorted
  }

  println("checkAnagramsCount")
  println(checkAnagramsCount("scala", "slaca"))
  println(checkAnagramsCount("scala", "alacs"))
  println(checkAnagramsCount("scala", "skala"))
  println(checkAnagramsCount("scala", "qwert"))
  println(checkAnagramsCount("scala", "sca"))

  println("checkAnagramsSort")
  println(checkAnagramsSort("scala", "slaca"))
  println(checkAnagramsSort("scala", "alacs"))
  println(checkAnagramsSort("scala", "skala"))
  println(checkAnagramsSort("scala", "qwert"))
  println(checkAnagramsSort("scala", "sca"))
}
