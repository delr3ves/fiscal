package com.emaginalabs.scala.redbook.chapter2

import scala.annotation.tailrec

object ArraySortChecker {

  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as match {
      case Array(a,b,_*) => ordered(a, b) && isSorted(as.tail, ordered)
      case _ => true
    }
  }

}
