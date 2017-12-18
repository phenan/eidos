package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait Semiringal [F[_]] extends Monoidal[F] {
  def append [A, B <: Coproduct] (fa: => F[A])(fb: => F[B]): F[A :+: B]
  def zero : F[CNil]

  def append3 [A, B, C <: Coproduct] (fa: => F[A])(fb: => F[B])(fc: => F[C]): F[A :+: B :+: C] = append(fa)(append(fb)(fc))
  def append4 [A, B, C, D <: Coproduct] (fa: => F[A])(fb: => F[B])(fc: => F[C])(fd: => F[D]): F[A :+: B :+: C :+: D] = append(fa)(append3(fb)(fc)(fd))

  def branch [A, B] (fa: => F[A], fb: => F[B]): F[A :+: B :+: CNil] = append3(fa)(fb)(zero)
  def branch [A, B, C] (fa: => F[A], fb: => F[B], fc: => F[C]): F[A :+: B :+: C :+: CNil] = append4(fa)(fb)(fc)(zero)
}
