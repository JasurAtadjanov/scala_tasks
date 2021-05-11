package scala2021.jatadjanov.task06

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.util.matching.Regex

object HW {


  val symbolPattern: Regex = "[^a-zA-Z]".r

  val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  case class Person(name: String, age: Int, email: String, sex: Sex.Value, height: Double)

  val p = Person("", 10, "j.atadjanov@gmail.com", Sex.Male, 100)

  def validateName(person: Person): Either[String, Boolean] = {
    symbolPattern.findFirstMatchIn(person.name) match {
      case Some(_) => Left(s"Name ${person.name} is invalid")
      case None => person.name.trim().isEmpty match {
        case false => Right(true)
        case true => Left(s"Name ${person.name} is invalid")
      }
    }
  }

  def validateEmail(person: Person): Either[String, Boolean] = person.email match {
    case null => Right(true)
    case person.email if person.email.trim.isEmpty => Right(true)
    case person.email if emailRegex.findFirstMatchIn(person.email).isDefined => Right(true)
    case _ => Left(s"Email ${person.email} is invalid")
  }

  def validateHeight(person: Person): Either[String, Boolean] = person.sex match {
    case Sex.Male if person.height > 100 => Right(true)
    case Sex.Male if person.height <= 100 => Left(s"Height ${person.height} is invalid")
    case _ => Right(true)
  }

  def validateAge(person: Person): Either[String, Boolean] = if (person.age > 0 && person.age < 100) {
    Right(true)
  } else {
    Left(s"Age ${person.age} is invalid")
  }

  def validateFirst(p: Person): Either[String, Boolean] = {
    val o = for {
      n <- validateName(p)
      v <- validateEmail(p)
      a <- validateAge(p)
      h <- validateHeight(p)
    } yield h;

    o
  }

  def validateFuture(p: Person): Unit = {
    val v = for {
      n <- Future (validateName(p))
      e <- Future (validateEmail(p))
      a <- Future (validateAge(p))
      h <- Future (validateHeight(p))
    } yield List(n, e, a, h)

    v.onComplete {
      case Success(value) => {
        val res = value.filter(f => f != Right(true)).map(f => f.left.getOrElse()).toList
        println(s"Validation result = $res")
      }
      case Failure(e) => e.printStackTrace
    }

    Thread.sleep(1000)

  }

  def main(args: Array[String]): Unit = {
    println(validateFirst(p));

    println(List(validateName(p), validateEmail(p), validateAge(p), validateHeight(p)).
      filter(f => f != Right(true)).map(f => f.left.getOrElse()).toList)

    validateFuture(p)

  }

}
