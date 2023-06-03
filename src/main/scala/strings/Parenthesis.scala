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

  def generateAllValidParentheses(n: Int): List[String] = {
    @tailrec
    def addNewParentheses(remaining: Set[String], newAcc: Set[String]): Set[String] = {
      if (remaining.isEmpty) newAcc
      else {
        val rs = remaining.head
        addNewParentheses(remaining.tail, newAcc ++ Set(rs + "()", "()" + rs, "(" + rs + ")"))
      }
    }
    @tailrec
    def applyTailrec(k: Int, acc: Set[String]): Set[String] = {
      if (k == n) acc
      else applyTailrec(k + 1, addNewParentheses(acc, Set.empty[String]))
    }
    applyTailrec(1, Set("()")).toList
  }

  println("hasValidParentheses")
  List("()", "()()", "(())", "(()())", ")", "(", ")(", "()(", "())", "(()", ")()").foreach(s => println(hasValidParentheses(s)))

  println("generateAllValidParentheses")
  (1 to 5).foreach(n => println(generateAllValidParentheses(n)))
}
