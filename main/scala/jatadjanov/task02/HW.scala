package scala2021.jatadjanov.task02

import scala.annotation.tailrec

object HW {

  def check(str: List[Char]): Boolean = {
    assert(str.nonEmpty, "Please enter symbols")

    val n = str.length

    val (start, end) = ('(', ')')

    @tailrec
    def f(idx: Int, count: Int = 0): Boolean = {
      (count < 0) match {
        case true => false
        case false if idx == n => count == 0
        case false if str(idx) == start => f(idx + 1, count + 1)
        case false if str(idx) == end => f(idx + 1, count - 1)
        case _ => f(idx + 1, count)
      }
    }

    f(0)
  }

  def main(args: Array[String]): Unit = {
    println(check("(1 + 2) * (3)".toList))
  }
}
