package com.emaginalabs.scala.redbook.chapter4

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class OptionTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  implicit val genIntArray = Gen.listOf(Gen.chooseNum(Int.MinValue, Int.MaxValue))

  implicit val genString = Gen.alphaNumStr
  val genPositive = Gen.choose(0, Int.MaxValue)

  "getOrElse" should "return the stored value when Some" in {
    forAll { (value: String) =>
      Some(value).getOrElse("anotherValue") shouldBe value
    }
  }

  it should "return the alternative value when None" in {
    forAll { (value: String) =>
      None.getOrElse(value) shouldBe value
    }
  }

  "orElse" should "return the original Option when Some" in {
    forAll { (value: String) =>
      Some(value).orElse(Some("anotherValue")) shouldBe Some(value)
    }
  }

  it should "return the alternative when None" in {
    forAll { (value: String) =>
      None.orElse(Some(value)) shouldBe Some(value)
    }
  }

  "map" should "transform the value inside the Some" in {
    forAll { (value: String) =>
      Some(value).map(_.length) shouldBe Some(value.length)
    }
  }

  it should "not transform anything when None" in {
    forAll { (value: String) =>
      None.map((x: String) => x.length) shouldBe None
    }
  }

  "filter" should "return Some when the value inside Some match the condition" in {
    forAll(genPositive) { (value: Int) =>
      Some(value).filter(_ >= 0) shouldBe Some(value)
    }
  }

  it should "return None when the value inside Some doesn't match the condition" in {
    forAll(genPositive) { (value: Int) =>
      Some(value).filter(_ < 0) shouldBe None
    }
  }

  it should "return None when None" in {
    None.filter((x: Int) => x >= 0) shouldBe None
  }

  "map2" should "return None when the first option is None" in {
    None.map2(Some("a"))((x: String, y: String) => x.length + y.length) shouldBe None
  }

  it should "return None when the second option is None" in {
    Some("a").map2(None)((x: String, y: String) => x.length + y.length) shouldBe None
  }

  it should "execute the map function over the two values" in {
    Some("hello").map2(Some("world"))((x: String, y: String) => s"$x $y") shouldBe Some("hello world")
  }

  "sequence" should "transform a List of Option to an option of List" in {
    forAll { (list: scala.List[Int]) =>
      val optionList = list.map(Some(_))
      Option.sequence(optionList) shouldBe Some(list)
    }
  }

  it should "transform a List of Option to an None when any None in the list" in {
    forAll { (list: scala.List[Int]) =>
      val optionList = insertRandomNone(list.map(Some(_)))
      Option.sequence(optionList) shouldBe None
    }
  }

  private def insertRandomNone(list: List[Option[Int]]): List[Option[Int]] = {
    val pos = Random.nextInt(list.length - 1 max 1) max 1
    val (front, back) = list.splitAt(pos)
    front ++ List(None) ++ back
  }
}
