package scala2021.jatadjanov.task04

import scala.annotation.tailrec


object HWImmutable {

  def checkCoins(vCoins: List[Int], total: Int): Boolean = {
    assert(total > 0, "Total should be greater than Zero")
    assert(vCoins != null && vCoins.nonEmpty, "The coins collection should not be empty")

    val coins = vCoins.sortWith((x, y) => x < y)
    val res = true :: List.fill(coins.head - 1)(false)

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

    f(total, coins.head + 1, res :+ true)
  }

  def main(args: Array[String]): Unit = {

  val coins = List.empty;
    println(checkCoins(null, 5))
  }
}
