package scala2021

import scala.annotation.tailrec

object Task2 {

  def check(str: List[Char]): Boolean = {
    assert(str.nonEmpty, "Please enter symbols")

    val n = str.length

    val (start, end) = ('(', ')')

    @tailrec
    def f(idx: Int, count: Int = 0): Boolean = if (count < 0 ) {
      false
    } else if (idx == n) count == 0 else if (str(idx) == start) {
      f(idx + 1, count + 1)
    } else if (str(idx) == end) {
      f(idx + 1, count - 1)
    } else {
      f(idx + 1, count)
    }

    f (0)
  }

  def main(args: Array[String]): Unit = {
      println(check("(1 + 2) * 3".toList))
  }
}
