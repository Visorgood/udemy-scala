package strings

object MultiplyStrings extends App {

  def addStrings(a: String, b: String): String = {
    val maxLen = math.max(a.length, b.length)
    val aExp = Seq.fill(maxLen - a.length + 1)('0').mkString + a
    val bExp = Seq.fill(maxLen - b.length + 1)('0').mkString + b
    val (_, res) = aExp.zip(bExp).foldRight((0, "")) {
      case ((ca: Char, cb: Char), (memory: Int, acc: String)) =>
        val x = (ca - '0') + (cb - '0') + memory
        val newMemory = if (x > 9) 1 else 0
        val newX = x % 10
        val newAcc = s"$newX$acc"
        (newMemory, newAcc)
    }
    if (res.head == '0') res.tail else res
  }

  def multiplyStringByDigit(a: String, b: Char): String = {
    val (restMemory, res) = a.foldRight((0, "")) {
      case (c: Char, (memory: Int, acc: String)) =>
        val x = (c - '0') * (b - '0') + memory
        val newMemory = x / 10
        val newX = x % 10
        val newAcc = s"$newX$acc"
        (newMemory, newAcc)
    }
    if (restMemory > 0) s"$restMemory$res" else res
  }

  def multiplyStrings(a: String, b: String): String = {
    val (_, res) = b.foldRight(("", "")) {
      case (c: Char, (zeros: String, acc: String)) =>
        val newStr = multiplyStringByDigit(a, c) + zeros
        val nextAcc = addStrings(newStr, acc)
        val nextZeros = zeros + "0"
        (nextZeros, nextAcc)
    }
    res
  }

  println("addStrings")
  println(addStrings("123", "45678"))
  println(addStrings("12345", "999"))
  println(addStrings("999", "999"))
  println(addStrings("23", "14"))

  println("multiplyStringByDigit")
  println(multiplyStringByDigit("123", '5'))
  println(multiplyStringByDigit("12345", '9'))
  println(multiplyStringByDigit("999", '9'))
  println(multiplyStringByDigit("5", '7'))

  println("multiplyStrings")
  println(multiplyStrings("1", "4"))
  println(multiplyStrings("123", "45678"))
  println(multiplyStrings("12345", "999"))
  println(multiplyStrings("999", "999"))
  println(multiplyStrings("23", "14"))
}
