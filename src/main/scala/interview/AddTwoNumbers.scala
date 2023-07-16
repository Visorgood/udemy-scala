package interview

import scala.annotation.tailrec

object AddTwoNumbers extends App {

  case class ListNode(x: Int, next: ListNode = null) {
    override def toString: String = x + (if (next == null) "" else next.toString)
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    def toList(l: ListNode): List[Int] = {
      var ln = l
      var lst = List.empty[Int]
      while (ln != null) {
        lst = ln.x :: lst
        ln = ln.next
      }
      lst.reverse
    }
    def fromList(lst: List[Int]): ListNode = {
      lst.reverse.foldLeft[ListNode](null)((res: ListNode, x: Int) => ListNode(x, res))
    }
    @tailrec
    def addNumbers(lst1: List[Int], lst2: List[Int], mem: Int, res: List[Int]): List[Int] = {
      if (lst1.isEmpty && lst2.isEmpty) {
        if (mem == 0) res.reverse
        else (mem :: res).reverse
      }
      else if (lst1.isEmpty) {
        val newDigit = lst2.head + mem
        addNumbers(lst1, lst2.tail, newDigit / 10, (newDigit % 10) :: res)
      }
      else if (lst2.isEmpty) {
        val newDigit = lst1.head + mem
        addNumbers(lst1.tail, lst2, newDigit / 10, (newDigit % 10) :: res)
      }
      else {
        val newDigit = lst1.head + lst2.head + mem
        addNumbers(lst1.tail, lst2.tail, newDigit / 10, (newDigit % 10) :: res)
      }
    }
    fromList(addNumbers(toList(l1), toList(l2), 0, List.empty))
  }

  println(addTwoNumbers(
    ListNode(2, ListNode(4, ListNode(9))),
    ListNode(8, ListNode(4, ListNode(4)))
  )) // 0931
  println(addTwoNumbers(
    ListNode(0),
    ListNode(0)
  )) // 0
  println(addTwoNumbers(
    ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9))))))),
    ListNode(9, ListNode(9, ListNode(9, ListNode(9))))
  )) // 89990001
}
