package com.emaginalabs.scala.redbook.chapter4

import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.util.Random

class EitherTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {


  implicit val genIntArray = Gen.listOf(Gen.chooseNum(Int.MinValue, Int.MaxValue))

  implicit val genString = Gen.alphaNumStr
  val genPositive = Gen.choose(0, Int.MaxValue)

  "orElse" should "return the stored value when Right" in {
    forAll { (value: String) =>
      Right(value).orElse(Right("anotherValue")) shouldBe Right(value)
    }
  }

  it should "return the alternative value when Left" in {
    forAll { (value: String) =>
      Left("any error").orElse(Right(value)) shouldBe Right(value)
    }
  }

  it should "return the alternative value when Left even if its another Left" in {
    forAll { (value: String) =>
      Left("any error").orElse(Left(value)) shouldBe Left(value)
    }
  }

  "map" should "transform the value inside the Right" in {
    forAll { (value: String) =>
      Right(value).map(_.length) shouldBe Right(value.length)
    }
  }

  it should "not transform anything when Left" in {
    forAll { (value: String) =>
      Left(5).map((x: String) => x.length) shouldBe Left(5)
    }
  }

  "map2" should "return Left when the first option is Left" in {
    Left(5).map2(Right("a"))((x: String, y: String) => x.length + y.length) shouldBe Left(5)
  }

  it should "return None when the second option is None" in {
    Right("a").map2(Left(5))((x: String, y: String) => x.length + y.length) shouldBe Left(5)
  }

  it should "execute the map function over the two values" in {
    Right("hello").map2(Right("world"))((x: String, y: String) => s"$x $y") shouldBe Right("hello world")
  }

  "sequence" should "transform a List of Option to an option of List" in {
    forAll { (list: scala.List[Int]) =>
      val optionList = list.map(Right(_))
      Either.sequence(optionList) shouldBe Right(list)
    }
  }

  it should "transform a List of Option to an None when any None in the list" in {
    forAll { (list: scala.List[Int]) =>
      val error = Left("error")
      val optionList = insertInListInRandomPosition(list.map(Right(_)), error)
      Either.sequence(optionList) shouldBe error
    }
  }

  private def insertInListInRandomPosition(list: List[Either[String, Int]],
                                           toInsert: Either[String, Int]): List[Either[String, Int]] = {
    val pos = Random.nextInt(list.length - 1 max 1) max 1
    val (front, back) = list.splitAt(pos)
    front ++ List(toInsert) ++ back
  }

}
