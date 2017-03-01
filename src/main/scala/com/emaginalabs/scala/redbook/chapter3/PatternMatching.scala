package com.emaginalabs.scala.redbook.chapter3

import com.emaginalabs.scala.redbook.chapter3.structures.{Cons, List, Nil}


object PatternMatching {

  val exercise3_1 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + 5
    case _ => 101
  }

}
