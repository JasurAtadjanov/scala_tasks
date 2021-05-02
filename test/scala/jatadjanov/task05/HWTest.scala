package scala2021.jatadjanov.task05

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HWTest extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {

  import HW.findManagerName

  test("check manager of John") {
    assert(findManagerName("John").isEmpty)
  }

  test("check manager of Mark") {
    assert(findManagerName("Mark").get == "Steve")
  }

  test("check manager of Steve") {
    assert(findManagerName("Steve").get == "Steve")
  }

  test("check manager of Igor") {
    assert(findManagerName("Igor").get == "Igor")
  }

  test("check manager of Christy") {
    assert(findManagerName("Christy").isEmpty)
  }

  test("check manager of Naveen") {
    assert(findManagerName("Naveen").isEmpty)
  }

  test("check manager of Megan") {
    assert(findManagerName("Megan").isEmpty)
  }
}
