package com.emaginalabs.scala.redbook.chapter2

object Curry {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a:A, b:B) => f(a)(b)

}
