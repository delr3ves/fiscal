package com.emaginalabs.scala.redbook.chapter6

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SimpleRNGTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  "the same generator" should "always generate the same number" in {
    forAll { (x: Long) =>
      val generator = SimpleRNG(x)
      generator.nextInt shouldBe generator.nextInt
    }
  }

  "nonNegativeInt" should "always generate a natural number" in {
    forAll { (x: Long) =>
      SimpleRNG(x).nonNegativeInt._1 should be >= 0
    }
  }

  "double" should "always generate a double between in range [0.0, 1.0)" in {
    forAll { (x: Long) =>
      val generated = SimpleRNG(x).double._1
      generated should be >= 0.0
      generated should be < 1.0
    }
  }

  "double3" should "always generate three different values" in {
    forAll { (x: Long) =>
      val generated = SimpleRNG(x).double3._1
      generated._1 should not be equal(generated._2)
      generated._1 should not be equal(generated._3)
      generated._2 should not be equal(generated._3)
    }
  }

  private val arbitraryMax = 1000
  "ints" should "generate a list of size n" in {
    forAll(Gen.choose(0, arbitraryMax)) { (x: Int) => //I use 1000 as an arbitrary max to not spend so long executing this test
      SimpleRNG(x).ints(x)._1.length shouldBe x
    }
  }

  it should "return a new generator" in {
    forAll(Gen.choose(0, arbitraryMax)) { (x: Int) =>
      val g1 = SimpleRNG(x)
      val (_, g2) = g1.ints(x)
      g1 should not be equal(g2)
    }
  }

}
