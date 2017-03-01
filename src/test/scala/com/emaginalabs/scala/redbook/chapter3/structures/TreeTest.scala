package com.emaginalabs.scala.redbook.chapter3.structures

import org.scalatest.{FlatSpec, Matchers}


class TreeTest extends FlatSpec with Matchers {

  val arbitraryTree = Branch(Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(2)), EmptyNode)), Branch(Leaf(34), Leaf(2)))

  "An empty tree" should "have 0 nodes" in {
    EmptyNode.size shouldBe 0
  }

  "An only left tree" should "have four nodes" in {
    Branch(Branch(Leaf(1), Leaf(2)), EmptyNode).size shouldBe 4
  }

  "An only right tree" should "have four nodes" in {
    Branch(EmptyNode, Branch(Leaf(1), Leaf(2))).size shouldBe 4
  }

  "An arbitrary populated" should "have few nodes" in {
    arbitraryTree.size shouldBe 10
  }

  "max" should "return the maximum value of a leaf" in {
    Tree.max(arbitraryTree) shouldBe 34
  }

  "depth" should "return the depth of a tree" in {
    arbitraryTree.depth shouldBe 5
  }

  "depth of leaf" should "be 1" in {
    Leaf(4).depth shouldBe 1
  }

  "depth of an empty list" should "be 0" in {
    EmptyNode.depth shouldBe 0
  }

  "map" should "trasnform every leaf" in {
    arbitraryTree.map((x: Int) => x.toString) shouldBe
      Branch(Branch(Leaf("1"), Branch(Branch(Leaf("1"), Leaf("2")), EmptyNode)), Branch(Leaf("34"), Leaf("2")))
  }

}
