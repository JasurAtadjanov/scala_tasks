package scala2021.jatadjanov.task03

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object HWImmutable {

  def encodeDirect(str: List[Char]): List[(Int, Char)] = {
    assert(str.nonEmpty, "Please enter symbols")

    val n = str.length - 1

    @tailrec
    def f(idx: Int, ch: Char, cnt: Int, result: Queue[(Int, Char)]): Queue[(Int, Char)] = {
      idx == n match {
        case true if ch == str(idx) => result :+ (cnt + 1, ch)
        case true => result :+ (cnt, ch) :+ (1, str(idx))
        case false if ch == str(idx) => f(idx + 1, ch, cnt + 1, result)
        case false => f(idx + 1, str(idx), 1, result :+ (cnt, ch))
      }
    }

    f(1, str(0), 1, Queue.empty).toList
  }

  def main(args: Array[String]): Unit = {
    println(encodeDirect("aaaabbbcaatt".toList))
  }
}
