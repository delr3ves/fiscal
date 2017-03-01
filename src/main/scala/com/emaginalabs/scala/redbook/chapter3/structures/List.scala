package com.emaginalabs.scala.redbook.chapter3.structures

import scala.annotation.tailrec

sealed trait List[+A] {

  def setHead[B >: A](h: B): List[B] = Cons(h, this)

  def head(): A

  def tail(): List[A]

  def drop(n: Long): List[A] = {
    this match {
      case Cons(h, t) if (n > 0) => t.drop(n - 1)
      case _ => this
    }
  }

  def dropWhile(f: (A) => Boolean): List[A] = {
    this match {
      case Cons(h, t) if (f(h)) => t.dropWhile(f)
      case _ => this
    }
  }

  def append[A](l2: List[A]): List[A] = this.foldRight(l2)((item: A, acc: List[A]) => acc.setHead(item))

  def add[A](item: A): List[A] = this.append(List(item))

  def init(): List[A] = this match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => List(h).append(t.init())
  }

  def length(): Long = this.foldLeft(0l)((acc: Long, _: A) => acc + 1)

  def foldRightNonTail[A, B](seed: B)(f: (A, B) => B): B = this.circuitBreakFoldR(seed)(f)((list) => false)

  def circuitBreakFoldR[A, B](seed: B)(f: (A, B) => B)(stop: (List[A]) => Boolean): B = this match {
    case Nil => seed
    case list: List[A] if (stop(list)) => f(list.head, seed)
    case Cons(h: A, t: List[A]) => f(h, t.circuitBreakFoldR(seed)(f)(stop))
  }

  def reverse(): List[A] = this.foldLeft(List[A]())((acc: List[A], item: A) => acc.setHead(item))

  def foldLeft[A, B](seed: B)(f: (B, A) => B): B = this match {
    case Nil => seed
    case Cons(h: A, t: List[A]) => t.foldLeft(f(seed, h))(f)
  }

  def foldRight[A, B](seed: B)(f: (A, B) => B): B = this.reverse().foldLeft(seed)((x: B, y: A) => f(y, x))


  def map[A, B](f: A => B): List[B] = this match {
    case Cons(h: A, t: List[A]) => Cons(f(h), t.map(f))
    case Nil => Nil
  }

  def filter[A](f: A => Boolean): List[A] = this.flatMap((x: A) => if (f(x)) List(x) else List())

  def flatMap[A, B](f: A => List[B]): List[B] = List.flattern(this.map(f))

  def zipWith[A, B, C](l2: List[B])(f: (A, B) => C): List[C] = (this, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x1: A, t1: List[A]), Cons(x2: B, t2: List[B])) => Cons(f(x1, x2), t1.zipWith(t2)(f))
  }

  def hasSubsequence[B >: A](search: List[B]) = {

    @tailrec
    def go(list: List[A], search: List[B]): Boolean = {
      (list, search) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(x1, t1), Cons(x2, t2)) if (x1 == x2) => go(t1, t2)
        case (Cons(_, t1), _) => go(t1, search)
      }
    }

    go(this, search)
  }
}

case object Nil extends List[Nothing] {
  override def tail(): List[Nothing] = Nil

  override def head(): Nothing = throw new IndexOutOfBoundsException()
}

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def flattern[A](list: List[List[A]]): List[A] = list.foldRight(List[A]())((item: List[A], acc: List[A]) => item.append(acc))

}


