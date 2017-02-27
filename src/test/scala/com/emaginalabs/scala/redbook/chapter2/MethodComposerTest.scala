package com.emaginalabs.scala.redbook.chapter2

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class MethodComposerTest extends FlatSpec with TableDrivenPropertyChecks with Matchers {

  "A composed function" should "be equals than execute the first method and then the seccond one" in {
    val doubleIt = (x:Int) => x * 2
    val stringify = (x: Int) => x.toString
    val composed = MethodComposer.compose(stringify, doubleIt)
    composed(4) shouldBe "8"
  }

}
