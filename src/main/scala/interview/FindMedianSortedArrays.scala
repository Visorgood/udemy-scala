package interview

import scala.annotation.tailrec

object FindMedianSortedArrays extends App {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    @tailrec
    def merge(rem1: Array[Int], rem2: Array[Int], res: List[Int]): Array[Int] = {
      if (rem1.isEmpty && rem2.isEmpty) res.reverse.toArray
      else if (rem1.isEmpty) merge(rem1, rem2.tail, rem2.head :: res)
      else if (rem2.isEmpty) merge(rem1.tail, rem2, rem1.head :: res)
      else if (rem1.head <= rem2.head) merge(rem1.tail, rem2, rem1.head :: res)
      else merge(rem1, rem2.tail, rem2.head :: res)
    }
    val mergedArray = merge(nums1, nums2, List.empty)
    val n = mergedArray.length
    if (n % 2 == 1) mergedArray(n / 2)
    else (mergedArray(n / 2 - 1) + mergedArray(n / 2)) / 2.0
  }

  println(findMedianSortedArrays(Array(1, 3), Array(2))) // 2
  println(findMedianSortedArrays(Array(1, 2), Array(3, 4))) // 2.5
  println(findMedianSortedArrays(Array(1, 2, 4, 7, 9), Array(2, 3, 6, 9, 12))) // 5
}
