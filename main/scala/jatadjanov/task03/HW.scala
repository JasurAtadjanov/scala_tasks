package scala2021.jatadjanov.task03

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuffer.empty

object HW {

  def encodeDirect(str: List[Char]): List[(Int, Char)] = {
    assert(str.nonEmpty, "Please enter symbols")

    val result: ArrayBuffer[(Int, Char)] = empty
    val n = str.length - 1

    def f(idx: Int, ch: Char, cnt: Int): Unit = {
      idx == n match {
        case true if ch == str(idx) => result.append((cnt + 1, ch))
        case true => {
          result.append((cnt, ch))
          result.append((1, str(idx)))
        }
        case false if ch == str(idx) => f(idx + 1, ch, cnt + 1)
        case false => {
          result.append((cnt, ch))
          f(idx + 1, str(idx), 1)
        }
      }
    }

    f(1, str(0), 1)
    result.toList
  }

  def main(args: Array[String]): Unit = {
    println(encodeDirect("aaaabbbcaatt".toList))
  }
}
