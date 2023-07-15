package interview

object TwoSum extends App {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val valsWithIndices = nums.zipWithIndex.foldLeft(Map.empty[Int, List[Int]]) {
      case (acc, (x, i)) =>
        val oldIndices = acc.getOrElse(x, List.empty)
        val newIndices = i :: oldIndices
        acc + (x -> newIndices)
    }
    val resOption = nums.find(x => {
      val yOption = valsWithIndices.get(target - x)
      yOption.nonEmpty && (x + x != target || yOption.get.length >= 2)
    })
    if (resOption.isEmpty) Array(-1, -1)
    else {
      val x = resOption.get
      if (x + x == target) {
        val indices = valsWithIndices(x)
        Array(indices.head, indices.tail.head)
      }
      else {
        val i = valsWithIndices(x).head
        val j = valsWithIndices(target - x).head
        Array(i, j)
      }
    }
  }

  println(twoSum(Array(2, 7, 3, 5, 1), 5).mkString(","))
  println(twoSum(Array(2, 7, 3, 5, 1), 6).mkString(","))
  println(twoSum(Array(2, 7, 3, 5, 10), 4).mkString(","))
  println(twoSum(Array(2, 7, 3, 5, 2), 4).mkString(","))
}
