package com.emaginalabs.scala.redbook.chapter5

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class StreamTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  implicit val genIntArray = Gen.listOf(Gen.chooseNum(Int.MinValue, Int.MaxValue))
  implicit val genStringArray = Gen.listOf(Gen.alphaNumStr)
  implicit val genPositive = Gen.chooseNum(1, Int.MaxValue)

  val lazyEvaluatedStream: Stream[Int] = Stream.cons(1, Stream.cons(sys.error("should not execute this"), Empty))

  "toList" should "put in a list the processed items of the stream" in {
    val i = 4
    val arbitraryStream = Stream(1 + 1, 2 + 2, 3 + 3, 5, i / 2)
    arbitraryStream.toList shouldBe List(2, 4, 6, 5, 2)
  }

  it should "get the original list in a complete round trip" in {
    forAll { (list: List[Int]) =>
      val arbitraryStream = Stream(list: _*)
      arbitraryStream.toList shouldBe list
    }
  }

  "take" should "put in a list the processed the n items" in {
    val arbitraryStream = Stream(1 + 1, 2 + 2, 3 + 3, 5)
    arbitraryStream.take(3).toList shouldBe List(2, 4, 6)
  }

  it should "not process the items from n+1 to Stream.length" in {
    lazyEvaluatedStream.take(1).toList shouldBe List(1)
  }

  it should "take no more than the length of the Stream" in {
    forAll { (list: List[Int]) =>
      val arbitraryStream = Stream(list: _*)
      arbitraryStream.take(list.length + 5).toList shouldBe list
    }
  }

  it should "should return the same list than takeToList" in {
    forAll { (list: List[Int], n: Int) =>
      val arbitraryStream = Stream(list: _*)
      arbitraryStream.take(n).toList shouldBe arbitraryStream.takeToList(n)
    }
  }

  "takeWhile" should "put in a list the processed  first matching conditions items" in {
    val arbitraryStream = Stream(1 + 1, 2 + 2, 3 + 3, 5)
    arbitraryStream.takeWhile(_ % 2 == 0).toList shouldBe List(2, 4, 6)
  }

  it should "not process the items after the first non-matching element" in {
    lazyEvaluatedStream.takeWhile(_ > 1).toList shouldBe Nil
  }

  it should "take no more than the length of the Stream" in {
    forAll { (list: List[Int]) =>
      val arbitraryStream = Stream(list: _*)
      arbitraryStream.takeWhile((_) => true).toList shouldBe list
    }
  }

  it should "take no elements when the first does not match the condition" in {
    forAll { (list: List[Int]) =>
      val arbitraryStream = Stream(list: _*)
      arbitraryStream.takeWhile((_) => false).toList shouldBe Nil
    }
  }

  "exists" should "not evaluate the complete stream if find the result" in {
    lazyEvaluatedStream.exists(_ <= 1) shouldBe true
  }

  it should "return false when no item match the condition" in {
    val arbitraryStream = Stream(1, 3, 5, 7, 9)
    arbitraryStream.exists(_ % 2 == 0) shouldBe false
  }

  it should "return false for an Empty stream" in {
    Empty.exists((x) => true) shouldBe false
  }

  "forAll" should "not evaluate the complete stream if find the result" in {
    lazyEvaluatedStream.forAll(_ > 1) shouldBe false
  }

  it should "return true when every item match the condition" in {
    val arbitraryStream = Stream(2, 4, 6, 8, 10)
    arbitraryStream.forAll(_ % 2 == 0) shouldBe true
  }

  it should "return true for an Empty stream" in {
    Empty.forAll((x) => false) shouldBe true
  }

  "foldableTakeWhile" should "generate a new Stream with n first items that match the criteria" in {
    val arbitraryStream = Stream(2, 4, 6, 8, 10, 11)
    arbitraryStream.foldableTakeWhile(_ % 2 == 0).toList shouldBe List(2, 4, 6, 8, 10)
  }

  it should "generate an empty Stream when the first elemetn does not match the criteria" in {
    val arbitraryStream = Stream(1, 2, 4, 6, 8, 10, 12)
    arbitraryStream.foldableTakeWhile(_ % 2 == 0).toList shouldBe List()
  }

  it should "generate an empty Stream when the input is empty" in {
    Stream[Int]().foldableTakeWhile(_ % 2 == 0).toList shouldBe List()
  }

  "headOption" should "return the first computed element when the stream is not empty" in {
    val arbitraryStream = Stream(1, 2, 3, 4, 5)
    arbitraryStream.headOption() shouldBe Some(1)
  }

  it should "only compute the firts element" in {
    lazyEvaluatedStream.headOption() shouldBe Some(1)
  }

  it should "return none the stream is empty" in {
    Stream().headOption() shouldBe None
  }

  "map" should "transform every item in the stream" in {
    val arbitraryStream = Stream(1, 2, 3, 4, 5)
    arbitraryStream.map(_.toString).toList shouldBe List("1", "2", "3", "4", "5")
  }

  "filter" should "exclude the items that not match the criteria" in {
    val arbitraryStream = Stream(1, 2, 3, 4, 5)
    arbitraryStream.filter(_ % 2 == 0).toList shouldBe List(2, 4)
  }

  "append" should "concatenate the two streams" in {
    val s1 = Stream(1, 2, 3, 4, 5)
    val s2 = Stream(6, 7, 8, 9)
    s1.append(s2).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  "flatMap" should "transform every item in the stream" in {
    val arbitraryStream = Stream(1, 2, 3, 4, 5)
    arbitraryStream.flatMap((x) => Stream(x, x)).toList shouldBe List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  }

  "constant" should "generate an infinite stream of a single item" in {
    Stream.constant("hi").take(5).toList shouldBe List("hi", "hi", "hi", "hi", "hi")
  }

  "from" should "generate an infinite stream starting from N" in {
    Stream.from(5).take(5).toList shouldBe List(5, 6, 7, 8, 9)
  }

  "fibs" should "generate fibonacci numbers" in {
    Stream.fibs.take(6).map(_._1).toList shouldBe List(0, 1, 1, 2, 3, 5)
  }

  "zipAll" should "join the two streams in a tuple" in {
    val expected = List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), Some("c")), (Some(4), Some("d")))
    Stream(1, 2, 3, 4).zipAll(Stream("a", "b", "c", "d")).toList shouldBe expected
  }

  it should "continue until the largest stream has elements" in {
    forAll { (numbers: List[Int], strings: List[String]) =>
      val numberStream = Stream(numbers: _*)
      val stringStream = Stream(strings: _*)
      numberStream.zipAll(stringStream).toList.length shouldBe (numbers.length max strings.length)
    }
  }

  "startsWith" should "return true when compare with itself" in {
    forAll { (strings: List[String]) =>
      val stringStream = Stream(strings: _*)
      stringStream.startsWith(stringStream) shouldBe true
    }
  }

  it should "return true when compare with a substream" in {
    forAll { (strings: List[String]) =>
      val stringStream = Stream(strings: _*)
      val length = Random.nextInt(strings.length + 1)
      stringStream.startsWith(stringStream.take(length)) shouldBe true
    }
  }

  it should "return false when the origianl stream is shorter than the prexif" in {
    forAll { (list: List[Int], n: Int) =>
      val nonEmptyList = list :+ n
      val nonEmptyListLength = nonEmptyList.length
      val randomShorterLength = nonEmptyListLength - Random.nextInt(nonEmptyListLength) - 1

      val longStream = Stream(nonEmptyList: _*)
      val shortStream = longStream.take(randomShorterLength)

      shortStream.startsWith(longStream) shouldBe false
    }
  }

  "tails" should "contain the list of intermediate streams" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  "scanRight" should "Store all intermedia operations" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
  }

  it should "perform every operation only once per element in the stream" in {
    forAll { (list: List[Int], n: Int) =>
      val stream = Stream(list: _*)

      var executedOperations = 0
      stream.scanRight(0)((x, y) => {
        executedOperations = executedOperations + 1
        x + y
      }).toList

      executedOperations shouldBe list.length
    }
  }

}
