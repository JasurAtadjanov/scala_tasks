package scala2021.jatadjanov.task04

import scala.annotation.tailrec

object HW {

  val checker = (vCoin: List[Int], vCount: List[Int], total: Int, idx: Int) => {
    var (s, i) = (total, idx)
    for ((coin, count) <- vCoin.zip(vCount)) {
      s = s - i % count * coin
      i = i / count
    }
    s
  }

  // Partial Applied
  val check = (vCoin: List[Int], total: Int) => {
    checker(vCoin, vCoin.map(x => total / x), total, _)
  }

  def checkCoins(coins: List[Int], total: Int): Boolean = {
    assert(total > 0, "Total should be greater than Zero")

    val ch = check(coins, total)
    // count of max combination
    val n = coins.map(x => total / x).sum

    @tailrec
  def f(idx: Int, n : Int, checker: Int => Int) :Boolean = {
      checker(idx) match {
        case 0 => true
        case _ if idx > n => false
        case _ => f(idx + 1, n, checker)
      }
  }

    f(0, n, ch)
  }

  def main(args: Array[String]): Unit = {
    val coins = List(2, 4, 6);
     println(checkCoins(coins, 10))
     println(checkCoins(coins, 11))
    println(checkCoins(coins, 12))
  }
}
