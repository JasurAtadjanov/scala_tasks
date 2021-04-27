package scala2021.jatadjanov.task01

import scala.collection.immutable

object HWImmutable extends App {

  val counts: Array[String] = Array(
    "12,ya.org",
    "1,abs.com",
    "900,google.com",
    "60,mail.yahoo.com",
    "10,mobile.sports.yahoo.com",
    "40,sports.yahoo.com",
    "10,stackoverflow.com",
    "2,en.wikipedia.org",
    "1,es.wikipedia.org",
    "1,mobile.sports"
  )

  val convertor = (item: String) => {
    val items = item.split(",")
    val cnt = items(0).toInt
    val s = items(1)

    val res: List[String] = s.split("\\.").tails.toList.slice(0, s.count(_ == '.') + 1).map(x => x.reduceLeft((i, j) => i + "." + j))
    res.map(i => (i -> cnt)).toMap
  }

  val collect = (map1: immutable.Map[String, Int], map2: immutable.Map[String, Int]) => {
    val r = map1.collect { case (k, v) => if (map2.contains(k)) (k, (v + map2(k))) else (k, v) }

    r.++(map2.filter(p => !r.contains(p._1)))
  }

  var p = counts.map(i => convertor(i)).reduceLeft((i, j) => collect(i, j))

  println(p)
}
