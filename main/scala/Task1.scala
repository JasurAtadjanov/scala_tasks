package scala2021

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Task1 {

  private val vAddsShow = mutable.HashMap.empty[String, ShowCount]
  private val rootName = ""
  private val separator = "."
  private val rootItem = new DomainShowCount(rootName)

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

  def getDomain(name: String): String = {
    if (name.contains(separator)) {
      name.substring(name.indexOf(separator) + 1)
    } else {
      rootName
    }
  }

  def fetch(item: String): (Int, String, String) = {
    val items: Array[String] = item.split(",")
    assert (items.length == 2, s"Data format is incorrect for: $item")

    val cnt: Int = try { items(0).toInt } catch {case _: Throwable => throw new RuntimeException(s"Data format is incorrect for: $item")}
    val name: String = items(1)
    val domain: String = getDomain(items(1))

    assert (cnt >= 0, s"Show count is incorrect for: $item")
    assert (domain != rootName, s"Show count is incorrect for: $item")

    (cnt, name, domain)
  }

  def append(cnt: Int, name: String): Unit = {
    //this.vAddsShow += this.vAddsShow.get(name).map(f => name -> f.incrShow(cnt)).getOrElse(name, new DomainShowCount(name, cnt))
    if (this.vAddsShow.contains(name)){
      this.vAddsShow(name).incrShow(cnt)
    } else {
      val item = new DomainShowCount(name)
      item.incrShow(cnt)
      this.vAddsShow.put(name, item)
    }
  }

  def calcCount(cnt: Int, name: String, domain: String): ShowCount = {
    val domainItem = if (domain == rootName){
      rootItem
    } else {
      calcCount(cnt, domain, getDomain(domain))
    }

    if (!this.vAddsShow.contains(name)) {
      append(cnt, name)
      domainItem.appendChild(this.vAddsShow(name))
    } else {
      append(cnt, name)
    }

    this.vAddsShow(name)
  }

  def main (args: Array[String]) {
    this.counts.foreach(item => {
      val (cnt, name, domain) = fetch(item)
      calcCount(cnt, name, domain)
    })

    // non sorted version
    this.vAddsShow.foreach(p => println(p._2))

    // sorted version
    println("------------------------")
    println("Sorted version")
    println("------------------------")
    this.rootItem.printChild()
  }
}

trait ShowCount {
  val name: String

  var cnt: Int = 0

  private val vChild = ArrayBuffer.empty[ShowCount]

  def incrShow(cnt: Int): Unit = this.cnt += cnt

  def appendChild(item: ShowCount): Unit = {
    this.vChild.append(item)
  }

  def printChild(sTab: String = ""): Unit = {
    this.vChild.sortWith((x, y) => x.cnt > y.cnt) foreach (ch => {
      println(sTab + ch)
      ch.printChild(sTab + "   ")
    })
  }

  override def toString = s"$cnt $name"
}

class DomainShowCount (val name: String) extends ShowCount
