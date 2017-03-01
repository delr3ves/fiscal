package com.emaginalabs.scala.redbook.chapter3.structures

sealed trait Tree[+A] {
  def size(): Long = fold((x: Int) => 1l)((x: Long, y: Long) => 1 + x + y)(() => 0l)

  def depth(): Long = fold((x: Int) => 1l)((x: Long, y: Long) => 1 + (x max y))(() => 0l)

  def map[A, B](f: A => B): Tree[B] = fold((x: A) => Leaf(f(x)): Tree[B])(Branch(_, _))(() => EmptyNode)

  def fold[A, B](leafHandler: A => B)(branchHandler: (B, B) => B)(emptyHandler: () => B): B = this match {
    case EmptyNode => emptyHandler()
    case Leaf(x: A) => leafHandler(x)
    case Branch(left, right) => branchHandler(
      left.fold(leafHandler)(branchHandler)(emptyHandler),
      right.fold(leafHandler)(branchHandler)(emptyHandler))
  }
}

object EmptyNode extends Tree[Nothing]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def max(tree: Tree[Int]): Int = tree match {
    case EmptyNode => Int.MinValue
    case Leaf(x) => x
    case Branch(left, right) => Tree.max(left) max Tree.max(right)
  }
}