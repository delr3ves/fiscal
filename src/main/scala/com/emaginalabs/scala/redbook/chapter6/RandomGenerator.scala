package com.emaginalabs.scala.redbook.chapter6

import scala.annotation.tailrec

trait RNG {

  def nextInt: (Int, RNG)

  def nonNegativeInt: (Int, RNG)

  def double: (Double, RNG)

  def intDouble: ((Int, Double), RNG)

  def doubleInt: ((Double, Int), RNG)

  def double3: ((Double, Double, Double), RNG)

  def ints(count: Int): (List[Int], RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  override def nonNegativeInt: (Int, RNG) = {
    val (x, rng) = nextInt
    (if (x < 0) -(x + 1) else x, rng)
  }

  override def double: (Double, RNG) = {
    val (x, rng) = nonNegativeInt

    def normalize(x: Int) = x.toDouble / Int.MaxValue.toDouble

    (if (x == Int.MaxValue) normalize(x - 1) else normalize(x), rng)
  }

  override def intDouble: ((Int, Double), RNG) = {
    val (int, gen1) = nextInt
    val (double, gen2) = gen1.double
    ((int, double), gen2)
  }

  override def doubleInt: ((Double, Int), RNG) = {
    val ((int, double), generator) = intDouble
    ((double, int), generator)
  }

  override def double3: ((Double, Double, Double), RNG) = {
    val (double1, gen1) = double
    val (double2, gen2) = gen1.double
    val (double3, gen3) = gen2.double
    ((double1, double2, double3), gen3)
  }

  override def ints(count: Int): (List[Int], RNG) = {
    @tailrec
    def generate(iters: Int, acc: List[Int], generator: RNG): (List[Int], RNG) = {
      if (iters > 0) {
        val (int, nextGen) = nextInt
        generate(iters - 1, int +: acc, nextGen)
      } else {
        (acc, generator)
      }
    }

    generate(count, List(), this)
  }
}