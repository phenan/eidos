package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait Monoidal[F[_]] {
  def product [A, B <: HList] (fa: => F[A])(fb: => F[B]): F[A :: B]
  def pure[A] (a: => A): F[A]

  def nil: F[HNil] = pure[HNil](HNil)
}
