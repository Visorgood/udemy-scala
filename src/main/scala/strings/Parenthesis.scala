package strings

import scala.annotation.tailrec

object Parenthesis extends App {

  def hasValidParentheses(s: String): Boolean = {
    @tailrec
    def applyTailrec(remaining: String, stack: List[Char]): Boolean = {
      if (remaining.isEmpty) stack.isEmpty
      else {
        val c = remaining.head
        if (c == '(') applyTailrec(remaining.tail, c :: stack)
        else if (c == ')' && stack.nonEmpty && stack.head == '(') applyTailrec(remaining.tail, stack.tail)
        else false
      }
    }
    applyTailrec(s, List.empty[Char])
  }

  println(hasValidParentheses("()"))
  println(hasValidParentheses("()()"))
  println(hasValidParentheses("(())"))
  println(hasValidParentheses("(()())"))
  println(hasValidParentheses(")"))
  println(hasValidParentheses("("))
  println(hasValidParentheses(")("))
  println(hasValidParentheses("()("))
  println(hasValidParentheses("())"))
  println(hasValidParentheses("(()"))
  println(hasValidParentheses(")()"))
}
