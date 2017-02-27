package com.emaginalabs.scala.redbook.chapter2

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

class FibonacciTest extends FlatSpec with TableDrivenPropertyChecks with Matchers {

  val fibonacciValues =
    Table(
      ("input", "expected"),
      (  1,   1),
      (  2,   1),
      (  3,   2),
      (  4,   3),
      (  5,   5),
      (  10,   55)
    )

  "Fibonacci: fib(n)" should "return fib(n-1) + fib(n-2)" in {
    forAll(fibonacciValues) { (input: Int, expected: Int) =>
      Fibonacci.fib(input) should be(expected)
    }
  }

  it should "not allow numbers lower than 1" in  {
    assertThrows[AssertionError] {
      Fibonacci.fib(0)
    }
  }

  "Message format" should "format the message for the given function" in {
    Fibonacci.formatResult("Fib of %d is %d", 10, Fibonacci.fib) shouldBe "Fib of 10 is 55"
  }
}
