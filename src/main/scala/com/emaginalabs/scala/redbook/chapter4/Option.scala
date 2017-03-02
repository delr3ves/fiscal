package com.emaginalabs.scala.redbook.chapter4

import scala.annotation.tailrec

trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option[B]): Option[B]

  def filter(f: A => Boolean): Option[A]

  def map2[A, B, C](b: Option[B])(f: (A, B) => C): Option[C] = (this, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(x1: A), Some(x2: B)) => Some(f(x1, x2))
  }

}

case class Some[+A](item: A) extends Option[A] {

  override def getOrElse[B >: A](default: => B): B = item

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this

  override def filter(f: (A) => Boolean): Option[A] = if (f(item)) this else None

  override def map[B](f: (A) => B): Option[B] = Some(f(item))

}

object None extends Option[Nothing] {

  override def getOrElse[B](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None

  override def map[B](f: (Nothing) => B): Option[B] = None

}

object Option {
  def variance(xs: Seq[Double]): Option[Double] = {
    xs match {
      case Nil => None
      case list => {
        val mean = list.sum / list.length
        val pow = list.map(x => math.pow(x - mean, 2))
        Some(pow.sum / pow.length)
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse[Option[A], A](a)((x:Option[A]) => x)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(list: List[A], acc: Option[List[B]], f: A => Option[B]): Option[List[B]] = list match {
      case Nil => acc
      case x :: xs if (f(x) == None) => None
      case x :: xs => go(xs, acc.map2(f(x))((l: List[B], v: B) => l :+ v), f)
    }

    go(a, Some(List.empty[B]), f)
  }


}