package com.emaginalabs.scala.redbook.chapter3.structures


import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class ListTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  implicit val genIntArray = Gen.listOf(Gen.chooseNum(Int.MinValue, Int.MaxValue))
  implicit val genInt = Gen.chooseNum(Int.MinValue, Int.MaxValue)
  val genNatural = Gen.chooseNum(0, Int.MaxValue)

  "Tail of an empty list" should "be the empty list" in {
    Nil.tail shouldBe Nil
  }

  "Tail of an populated list" should "be the list without the head" in {
    List(1, 2, 3).tail shouldBe List(2, 3)
  }

  "Length" should "return the number of element in the list" in {
    forAll { (scalaList: scala.List[Int], head: Int) =>
      val myList: List[Int] = List(scalaList: _*)
      myList.length shouldBe scalaList.length
    }
  }

  "Set head" should "should add the head to an empty list" in {
    Nil.setHead(1) shouldBe List(1)
  }

  it should "should add the head to a populated list" in {
    List(1, 2, 3).setHead(0) shouldBe List(0, 1, 2, 3)
  }

  it should "should add the element into first place" in {
    forAll { (list: scala.List[Int], head: Int) =>
      List(list: _*).setHead(head).head shouldBe head
    }
  }

  it should "increase the length of the list by 1" in {
    forAll { (list: scala.List[Int], head: Int) =>
      List(list: _*).setHead(head).length shouldBe list.length + 1
    }
  }

  "drop" should "reduce the number of elements of the list by n" in {
    List(1, 2, 3, 4).drop(2) shouldBe List(3, 4)
  }

  it should "should return empty list when drop more than the elements in the list" in {
    forAll { (scalaList: scala.List[Int]) =>
      val list = List(scalaList: _*)
      list.drop(list.length + 1) shouldBe Nil
    }
  }

  it should "decrease the length of the list by n but never be lower than zero" in {
    forAll(genIntArray, genNatural) { (scalaList: scala.List[Int], n: Int) =>
      val list = List(scalaList: _*)
      list.drop(n).length shouldBe Math.max(0, list.length - n)
    }
  }

  "dropWhile" should "reduce the number of elements of the list by while the condition is true" in {
    List(1, 2, 3, 4).dropWhile((x) => x <= 2) shouldBe List(3, 4)
  }

  "append" should "concatenate two lists" in {
    List(1, 2).append(List(3, 4)) shouldBe List(1, 2, 3, 4)
  }

  "init in a populate list" should "discard the last element" in {
    List(1, 2, 3, 4).init() shouldBe List(1, 2, 3)
  }

  "init in a empty list" should "return the empty" in {
    List().init shouldBe Nil
  }

  "foldRight" should "reduce the list" in {
    List(1, 2, 3, 4).foldRight(0)((x: Int, y: Int) => x + y) shouldBe (10)
  }

  it should "reduce the list by right" in {
    List(1, 2, 3, 4).foldRight(0)((x: Int, y: Int) => x - y) shouldBe (-2)
  }

  it should "return the seed on empty list" in {
    val seed = 5
    List().foldRight(seed)((x: Int, y: Int) => x + y) shouldBe (seed)
  }

  "circuitBreakFoldR" should "reduce the list and take the " in {
    List(1, 1, 3, 4).circuitBreakFoldR(1)((x: Int, y: Int) => x * y)(_.head == 0) shouldBe (12)
  }

  it should "stop the computatin after processing the element that match the stop condition" in {
    List(1, 1, 3, 4).circuitBreakFoldR(1)((x: Int, y: Int) => x * y)(_.head == 3) shouldBe (3)
  }

  it should "return the seed when the list is empty" in {
    List().circuitBreakFoldR(1)((x: Int, y: Int) => x * y)(_.head == 0) shouldBe (1)
  }

  "foldLeft" should "reduce the list" in {
    List(1, 2, 3, 4).foldLeft(0)((x: Int, y: Int) => x + y) shouldBe (10)
  }

  it should "return the seed on empty list" in {
    val seed = 5
    List().foldLeft(seed)((x: Int, y: Int) => x - y) shouldBe seed
  }

  it should "operate from the left" in {
    List(1, 2, 3, 4).foldLeft(0)((x: Int, y: Int) => x - y) shouldBe (-10)
  }

  it should "return the same resutl than foldRight on commutative operations" in {
    forAll(genIntArray, genNatural) { (scalaList: scala.List[Int], n: Int) =>
      val list = List(scalaList: _*)
      val op = (x: Int, y: Int) => x + y
      list.foldLeft(0)(op) shouldBe list.foldRight(0)(op)
    }
  }

  "reverse" should "return the same list in reverse order" in {
    List(1, 2, 3, 4, 5).reverse() shouldBe List(5, 4, 3, 2, 1)
  }

  it should "return the empty list when the input is empty" in {
    List().reverse() shouldBe List()
  }

  "flattern" should "plain the list of lists" in {
    List.flattern(List(List(1, 2, 3), List(3), List(4, 5, 6), List())) shouldBe List(1, 2, 3, 3, 4, 5, 6)
  }

  it should "return the empty list when the input is empty" in {
    List.flattern(List()) shouldBe List()
  }

  it should "return the empty list when the input only contains empty lists" in {
    List.flattern(List(Nil, Nil, Nil, Nil)) shouldBe List()
  }

  "map" should "transform every value form the original list" in {
    List(1, 2, 3, 4, 5).map((x: Int) => x * 2) shouldBe List(2, 4, 6, 8, 10)
  }

  it should "update the type when transformation change the return type" in {
    List(1, 2, 3, 4, 5).map((x: Int) => x.toString) shouldBe List("1", "2", "3", "4", "5")
  }

  "filter" should "exclude all elements that does not match the criteira" in {
    List(1, 2, 3, 4, 5, 6).filter((x: Int) => x % 2 == 0) shouldBe List(2, 4, 6)
  }

  it should "not exclude any element when all of them match the criteria" in {
    List(1, 2, 3, 4, 5, 6).filter((x: Int) => true) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  it should "exclude any element when none of them match the criteria" in {
    List(1, 2, 3, 4, 5, 6).filter((x: Int) => false) shouldBe List()
  }

  it should "return the empty list when the input is empty" in {
    List().filter((x: Int) => true) shouldBe List()
  }

  "flatMap" should "transform every value form the original list" in {
    List(1, 2, 3, 4, 5).flatMap((x: Int) => List(x * 2)) shouldBe List(2, 4, 6, 8, 10)
  }

  "zipWith" should "comnbine two lists" in {
    List(1, 2, 3).zipWith(List(4, 5, 6))((x: Int, y: Int) => x + y) shouldBe List(5, 7, 9)
  }

  it should "generate a list with a lenght with the shorted list" in {
    forAll { (sl1: scala.List[Int], sl2: scala.List[Int]) =>
      val l1 = List(sl1: _*)
      val l2 = List(sl2: _*)
      l1.zipWith(l2)((x: Int, y: Int) => x + y).length shouldBe Math.min(sl1.length, sl2.length)
    }
  }

  "hasSubsequence" should "return true when l2 is contained in orignal list" in {
    List(1, 2, 3, 4).hasSubsequence(List(2, 3)) shouldBe true
  }

  it should "return true true when both lists are empty" in {
    List().hasSubsequence(List()) shouldBe true
  }

  it should "return true when finding the empty list" in {
    List("a", "b", "c").hasSubsequence(List()) shouldBe true
  }

  it should "return false when the original list is empty" in {
    List().hasSubsequence(List(3, 3, 4)) shouldBe false
  }

  it should "return false when l2 is not contained in orignal list" in {
    List(1, 2, 3, 4).hasSubsequence(List(3, 3, 4)) shouldBe false
  }
}
