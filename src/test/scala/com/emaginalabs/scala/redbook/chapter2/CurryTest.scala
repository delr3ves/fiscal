package com.emaginalabs.scala.redbook.chapter2

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class CurryTest extends FlatSpec with TableDrivenPropertyChecks with Matchers {

  val testCases =
    Table(
      ("function", "value1", "value2"),
      ((x: Int, y: Int) => x + y, 1, 2),
      ((x: Int, y: Int) => x - y, 1, 2),
      ((x: Int, y: Int) => x / y, 1, 2),
      ((x: Int, y: Int) => x * y, 1, 2)
    )

  "A curryfied function" should "return the same than original function" in {
    forAll(testCases) { (f: (Int, Int) => Int, x: Int, y: Int) =>
      val curryfied = Curry.curry(f)
      f(x, y) shouldBe (curryfied(x)(y))
    }
  }

  "An uncurryfied prevously curryfied function" should "should return the same than original function" in {
    forAll(testCases) { (f: (Int, Int) => Int, x: Int, y: Int) =>
      val curryfied = Curry.curry(f)
      val uncurryfied = Curry.uncurry(curryfied)
      f(x, y) shouldBe (uncurryfied(x, y))
    }
  }

  it should "should return the same than the curryfied function" in {
    forAll(testCases) { (f: (Int, Int) => Int, x: Int, y: Int) =>
      val curryfied = Curry.curry(f)
      val uncurryfied = Curry.uncurry(curryfied)
      curryfied(x)(y) shouldBe (uncurryfied(x, y))
    }
  }

}
