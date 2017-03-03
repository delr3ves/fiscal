package com.emaginalabs.scala.redbook.chapter5

import com.emaginalabs.scala.redbook.chapter5.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def toList: List[A] = {
    @tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Empty => acc
      case Cons(h, t) => go(t(), acc :+ h())
    }

    go(this, Nil)
  }

  def take(n: Int): Stream[A] = {
    def unfoldTake(x: (Stream[A], Int)): Option[(A, (Stream[A], Int))] = x match {
      case (_, x) if x < 1 => None
      case (Empty, _) => None
      case (Cons(h, t), left) => Some(h(), (t(), left - 1))
    }

    unfold((this, n))(unfoldTake)
  }

  def takeToList(n: Int): List[A] = {
    @tailrec
    def go(es: Stream[A], acc: List[A], n: Int): List[A] = es match {
      case Cons(h, t) if n > 0 => go(t(), acc :+ h(), n - 1)
      case _ => acc
    }

    go(this, Nil, n)
  }

  def takeWhile(cond: (A) => Boolean): Stream[A] = {
    def unfoldTake(x: Stream[A]): Option[(A, Stream[A])] = x match {
      case Cons(h, t) if (cond(h())) => Some((h(), t()))
      case _ => None
    }

    unfold(this)(unfoldTake)
  }

  def takeWhileToList(cond: (A) => Boolean): List[A] = {
    @tailrec
    def go(es: Stream[A], acc: List[A], cond: (A) => Boolean): List[A] = es match {
      case Cons(h, t) if cond(h()) => go(t(), acc :+ h(), cond)
      case _ => acc
    }

    go(this, Nil, cond)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def foldableTakeWhile(p: (A) => Boolean): Stream[A] =
    foldRight(empty[A])((x, acc) => if (p(x)) cons(x, acc) else empty)

  def exists(p: A => Boolean): Boolean = foldRight(false)((x, y) => p(x) || y)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, y) => p(x) && y)

  def headOption(): Option[A] = foldRight(None: Option[A])((x, res) => Some(x))

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((x, acc) => cons(x, acc))


  def map[B](f: (A) => B): Stream[B] = {
    def unfoldMap(es: Stream[A]): Option[(B, Stream[A])] = es match {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

    unfold(this)(unfoldMap)
  }

  def foldableMap[B](f: (A) => B): Stream[B] = foldRight(empty[B])((x, acc) => cons(f(x), acc))

  def flatMap[B](f: (A) => Stream[B]): Stream[B] = foldRight(empty[B])((x, acc) => f(x) append acc)

  def filter(f: (A) => Boolean): Stream[A] = foldRight(empty[A])((x, acc) => if (f(x)) cons(x, acc) else acc)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    def unfoldZip(es: (Stream[A], Stream[B])): Option[((Option[A], Option[B]), (Stream[A], Stream[B]))] = es match {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Empty, Empty) => None
    }

    unfold((this, s2))(unfoldZip)
  }


  def startsWith[A](s: Stream[A]): Boolean = this.zipAll(s)
    .takeWhile((x) => x._2.isDefined)
    .forAll((x) => x._1 == x._2)

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case current@Cons(h, t) => Some((current, t()))
  }.append(Stream(Empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z)))((x, a) => {
    lazy val acc = a
    val newSeed = f(x, acc._1)
    (newSeed, cons(newSeed, acc._2))
  })._2

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = unfold(a)((x) => Some(x, x))

  def from(n: Int): Stream[Int] = unfold(n)((x) => Some(x, x + 1))

  val fibs = unfold((0, 1))((x) => Some((x._1, x._2), (x._2, x._1 + x._2)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((item: A, seed: S)) => cons(item, unfold(seed)(f))
  }

}