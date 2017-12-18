package com.phenan.eidos

import util.coproduct._

import shapeless._

import scala.language.higherKinds

trait InvariantSemiringal [F[_]] extends Semiringal[F] with InvariantMonoidal[F] {

  import syntax.invariantSemiringal._

  implicit private def self: InvariantSemiringal[F] = this

  def rep [A]: F[A] => F[List[A]] = fa => union [List[A]] (
    struct[scala.::[A]](fa, rep(fa)),
    struct[Nil.type]()
  )

  def optional [A]: F[A] => F[Option[A]] = fa => union [Option[A]] (
    struct[Some[A]](fa),
    struct[None.type]()
  )

  def select [A, R <: Coproduct, T] (f: => F[A]) (implicit aux: Generic.Aux[T, R], eq: (A :+: CNil) =+= R): F[T] = {
    (f +: zero).xmap(eq._1.translate)(eq._2.translate).wrap
  }
  def select [A1, A2, R <: Coproduct, T] (f1: => F[A1], f2: => F[A2]) (implicit aux: Generic.Aux[T, R], eq: (A1 :+: A2 :+: CNil) =+= R): F[T] = {
    (f1 +: f2 +: zero).xmap(eq._1.translate)(eq._2.translate).wrap
  }
  def select [A1, A2, A3, R <: Coproduct, T] (f1: => F[A1], f2: => F[A2], f3: => F[A3]) (implicit aux: Generic.Aux[T, R], eq: (A1 :+: A2 :+: A3 :+: CNil) =+= R): F[T] = {
    (f1 +: f2 +: f3 +: zero).xmap(eq._1.translate)(eq._2.translate).wrap
  }
}
