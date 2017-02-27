package com.emaginalabs.scala.redbook.chapter2

object MethodComposer {

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}
