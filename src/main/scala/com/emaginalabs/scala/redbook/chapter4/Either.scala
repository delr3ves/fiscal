package com.emaginalabs.scala.redbook.chapter4

import scala.annotation.tailrec

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B): Either[E, B] = this

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {

  override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b match {
    case Right(v) => Right(f(value, v))
    case left@Left(e) => Left(e)
  }

}

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)((e: Either[E, A]) => e)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def go(list: List[A], acc: Either[E, List[B]], f: A => Either[E, B]): Either[E, List[B]] = list match {
      case Nil => acc
      case x :: xs => f(x) match {
        case error@Left(e) => error
        case rigth@Right(v) => go(xs, acc.map2(rigth)((l: List[B], v: B) => l :+ v), f)
      }
    }

    go(as, Right(List.empty[B]), f)
  }

}