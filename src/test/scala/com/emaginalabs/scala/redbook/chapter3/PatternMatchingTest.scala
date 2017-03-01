package com.emaginalabs.scala.redbook.chapter3

import org.scalatest.{FlatSpec, Matchers}

class PatternMatchingTest extends FlatSpec with Matchers {

  "val exercise3_1" should "execute the first clasuse that match" in {
    PatternMatching.exercise3_1 shouldBe 3
  }

}
