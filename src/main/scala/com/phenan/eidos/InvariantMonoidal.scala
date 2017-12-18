package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait InvariantMonoidal [F[_]] extends Monoidal[F] with InvariantFunctor[F] {

  import syntax.monoidal._
  import syntax.invariantFunctor._

  private implicit def self: InvariantMonoidal[F] = this

  def *> [A] : F[Unit] => F[A] => F[A] = f_ => fa => {
    product(f_)(fa *: nil).xmap(_.tail.head)(() :: _ :: HNil)
  }

  def *< [A] : F[A] => F[Unit] => F[A] = fa => f_ => {
    product(fa)(f_ *: nil).xmap(_.head)(_ :: () :: HNil)
  }

  def *>: [A <: HList] : F[Unit] => F[A] => F[A] = f_ => fa => {
    product(f_)(fa).xmap(_.tail)(() :: _)
  }

  def *<: [A, B <: HList] : F[A] => F[Unit :: B] => F[A :: B] = fa => f_b => {
    product(fa)(f_b).xmap { case a :: _ :: b => a :: b } { case a :: b => a :: () :: b }
  }

  def construct [T] (implicit aux: Generic.Aux[T, HNil]): F[T] = nil.wrap
  def construct [A, T] (f: => F[A])(implicit aux: Generic.Aux[T, A :: HNil]): F[T] = (f *: nil).wrap
  def construct [A1, A2, T] (f1: => F[A1], f2: => F[A2])(implicit aux: Generic.Aux[T, A1 :: A2 :: HNil]): F[T] = (f1 *: f2 *: nil).wrap
  def construct [A1, A2, A3, T] (f1: => F[A1], f2: => F[A2], f3: => F[A3])(implicit aux: Generic.Aux[T, A1 :: A2 :: A3 :: HNil]): F[T] = (f1 *: f2 *: f3 *: nil).wrap
}
