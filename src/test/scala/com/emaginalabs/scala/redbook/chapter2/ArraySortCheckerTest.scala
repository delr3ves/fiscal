package com.emaginalabs.scala.redbook.chapter2

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ArraySortCheckerTest extends FlatSpec with TableDrivenPropertyChecks with Matchers {

  val emptyOrSingleArray =
    Table(
      ("array"),
      (Array(1)),
      (Array())
    )

  val ascSortedArrays =
    Table(
      ("array"),
      (Array(1, 7)),
      (Array(1, 5, 7, 11))
    )

  val unsortedArrays =
    Table(
      ("array"),
      (Array('b', 'c', 'a')),
      (Array('a', 'b', 'a')),
      (Array('a', 'c', 'b'))
    )

  "An empty or single element array" should "be sorted with any function" in {
    forAll(ascSortedArrays) { (as: Array[Int]) =>
      ArraySortChecker.isSorted(as, (x:Int, y:Int) => x < y) shouldBe true
    }
  }

  "An asc sorted array" should "be sorted with lower sort function" in {
    forAll(ascSortedArrays) { (as: Array[Int]) =>
      ArraySortChecker.isSorted(as, (x:Int, y:Int) => x < y) shouldBe true
    }
  }

  it should "not be sorted with greater sort function" in {
    forAll(ascSortedArrays) { (as: Array[Int]) =>
      ArraySortChecker.isSorted(as, (x:Int, y:Int) => x > y) shouldBe false
    }
  }

  "An unsorted array" should "not be sorted with greater lower function" in {
    forAll(unsortedArrays) { (as: Array[Char]) =>
      ArraySortChecker.isSorted(as, (x:Char, y:Char) => x < y) shouldBe false
    }
  }

}
