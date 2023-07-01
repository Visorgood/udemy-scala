package InterestingProblems

object Eval extends App {

  def eval(expr: String): Int = {
    val exprWithoutMinuses = expr.replace(" - ", " + -1 * ")
    val multOrDivBlocks = exprWithoutMinuses.split(" \\+ ")
    multOrDivBlocks.foldLeft(0) {
      case (res: Int, block: String) =>
        val values = block.split(" [\\*\\/] ").map(_.toInt)
        val ops = block.filter(c => c == '*' || c == '/')
        val newVal = values.tail.zip(ops).foldLeft(values.head) {
          case (acc: Int, (v: Int, op: Char)) => if (op == '*') acc * v else acc / v
        }
        res + newVal
    }
  }

  println(eval("3 + 2 * 6 / 3 - 4 * 2 + 6 / 2 - 9 / 3 * 1 + 9")) // 8
  println(eval("1 + 2 * 3 + 4 / 5 + 6 * 7 - 8")) // 41
}
