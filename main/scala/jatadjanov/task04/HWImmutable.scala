package scala2021.jatadjanov.task04

import scala.annotation.tailrec


object HWImmutable {

  def checkCoins(vCoins: List[Int], total: Int): Boolean = {
    assert(total >= 0, "Total should not be negative")
    assert(vCoins != null, "The coins collection should not be NULL")
    val coins = vCoins.sortWith((x, y) => x < y)

    def check (coin: Int, idx: Int, res: List[Boolean]): Boolean = {
      (coin <= idx) match {
        case true => res(idx - coin)
        case false => false
      }
    }

    @tailrec
    def f(total: Int, idx: Int, res: List[Boolean]) :Boolean = {
      val result: Boolean = coins.foldLeft(false)((x, y) => x || check(y, idx, res))
      (total == idx) match {
        case true => result
        case _ => f(total, idx + 1, res :+ result)
      }
    }

    (vCoins.nonEmpty && total > 0) match {
      case true => {
        val res = true :: List.fill(coins.head - 1)(false)
        f(total, coins.head + 1, res :+ true)
      }
      case false => total == 0
    }
  }

  def main(args: Array[String]): Unit = {

  val coins = List.empty;
    println(checkCoins(coins, 1))
  }
}
