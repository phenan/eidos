package com.phenan.eidos
package syntax

import shapeless._

import scala.language.higherKinds

trait InvariantMonoidalSyntax {
  implicit class InvariantMonoidalOps1 [F[_], A <: HList] (fa: => F[A])(implicit F: InvariantMonoidal[F]) {
    def *>: (f_ : F[Unit]): F[A] = F.*>:(f_)(fa)
  }
  implicit class InvariantMonoidalOps2 [F[_], A <: HList] (fa: => F[Unit :: A])(implicit F: InvariantMonoidal[F]) {
    def *<: [B] (fb : F[B]): F[B :: A] = F.*<:(fb)(fa)
  }
  implicit class InvariantMonoidalOps3 [F[_]] (f_ : => F[Unit])(implicit F: InvariantMonoidal[F]) {
    def *> [A] (fa : F[A]): F[A] = F.*>(f_)(fa)
  }
  implicit class InvariantMonoidalOps4 [F[_], A] (fa : => F[A])(implicit F: InvariantMonoidal[F]) {
    def *< (f_ : F[Unit]): F[A] = F.*<(fa)(f_)
  }

  def struct [A]: Multiplier[A] = new Multiplier

  class Multiplier [T] {
    def apply [F[_]] () (implicit aux: Generic.Aux[T, HNil], F: InvariantMonoidal[F]): F[T] = F.construct
    def apply [F[_], A] (f: => F[A]) (implicit aux: Generic.Aux[T, A :: HNil], F: InvariantMonoidal[F]): F[T] = F.construct(f)
    def apply [F[_], A1, A2] (f1: => F[A1], f2: => F[A2]) (implicit aux: Generic.Aux[T, A1 :: A2 :: HNil], F: InvariantMonoidal[F]): F[T] = F.construct(f1, f2)
    def apply [F[_], A1, A2, A3] (f1: => F[A1], f2: => F[A2], f3: => F[A3]) (implicit aux: Generic.Aux[T, A1 :: A2 :: A3 :: HNil], F: InvariantMonoidal[F]): F[T] = F.construct(f1, f2, f3)
  }
}

object InvariantMonoidalSyntax extends InvariantMonoidalSyntax
