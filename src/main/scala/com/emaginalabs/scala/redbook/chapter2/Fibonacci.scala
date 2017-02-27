package com.emaginalabs.scala.redbook.chapter2

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = {
    assert(n > 0)
    @tailrec
    def loop(a: Int, b: Int, n: Int): Int = {
      if (n < 1) {
        a
      } else {
        loop(b, a + b, n -1)
      }
    }

    loop(0, 1, n)
  }

  def formatResult(msg: String, n: Int, f: (Int) => Int): String = msg.format(n, f(n))
}
