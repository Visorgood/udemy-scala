package InterestingProblems

import scala.annotation.tailrec

object NQueens extends App {

  def nQueens(n: Int): List[List[Int]] = {
    val nNums = (0 until n).toList
    def removeCurrentQueen(acc: List[(Int, List[Int])]): List[(Int, List[Int])] = {
      if (acc.isEmpty) acc
      else (acc.head._1, acc.head._2.tail) :: acc.tail
    }
    def addToResult(acc: List[(Int, List[Int])], res: List[List[Int]]): List[List[Int]] = {
      if (acc.size < n) res
      else acc.map(_._2.head).reverse :: res
    }
    def computeNextRow(acc: List[(Int, List[Int])]): (Int, List[Int]) = {
      if (acc.size == n) (-1, List.empty)
      else {
        val nextRowIndex = acc.head._1 + 1
        val occupiedCells = acc.map(p => (p._1, p._2.head))
        val nextRowQueens = nNums.filterNot(s => occupiedCells.exists(c => s == c._2 || nextRowIndex - c._1 == math.abs(s - c._2)))
        (nextRowIndex, nextRowQueens)
      }
    }
    @tailrec
    def nQueensTailrec(acc: List[(Int, List[Int])], res: List[List[Int]]): List[List[Int]] = {
      if (acc.isEmpty) res.reverse
      else if (acc.head._2.isEmpty) nQueensTailrec(removeCurrentQueen(acc.tail), res)
      else {
        val (nextRowIndex, nextRowQueens) = computeNextRow(acc)
        if (nextRowQueens.isEmpty) nQueensTailrec(removeCurrentQueen(acc), addToResult(acc, res))
        else nQueensTailrec((nextRowIndex, nextRowQueens) :: acc, res)
      }
    }
    nQueensTailrec(List((0, nNums)), List.empty)
  }

  (1 to  8).foreach(k => {
    val res = nQueens(k)
    println(s"nQueens($k) has ${res.size} solutions:")
    res.foreach(println)
    println()
  })
}
