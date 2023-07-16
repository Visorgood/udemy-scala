package interview

import scala.annotation.tailrec

object LengthOfLongestSubstring extends App {
  def lengthOfLongestSubstring(s: String): Int = {
    @tailrec
    def applyTailrec(curIndex: Int, startIndexOfCurWindow: Int, chars: Map[Char, Int], maxLength: Int): Int = {
      if (curIndex == s.length) math.max(maxLength, curIndex - startIndexOfCurWindow)
      else {
        val c = s(curIndex)
        val charIndex = chars.get(c)
        val charIsAlreadyInCurWindow = charIndex.isDefined && charIndex.get >= startIndexOfCurWindow
        val (newStartIndexOfCurWindow, newMaxLength) =
          if (charIsAlreadyInCurWindow) (charIndex.get + 1, math.max(maxLength, curIndex - startIndexOfCurWindow))
          else (startIndexOfCurWindow, maxLength)
        applyTailrec(curIndex + 1, newStartIndexOfCurWindow, chars + (c -> curIndex), newMaxLength)
      }
    }
    applyTailrec(0, 0, Map.empty, 0)
  }

  println(lengthOfLongestSubstring("abcabcbb")) // 3
  println(lengthOfLongestSubstring("bbbbb")) // 1
  println(lengthOfLongestSubstring("pwwkew")) // 3
  println(lengthOfLongestSubstring("abba")) // 2
}
