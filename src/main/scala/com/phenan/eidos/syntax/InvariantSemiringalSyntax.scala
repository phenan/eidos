package com.phenan.eidos
package syntax

import util.coproduct._

import shapeless._

import scala.language.higherKinds

trait InvariantSemiringalSyntax {
  implicit class InvariantSemiringalOps [F[_], A] (fa: => F[A]) (implicit F: InvariantSemiringal[F]) {
    def rep : F[List[A]] = F.rep(fa)
    def ? : F[Option[A]] = F.optional(fa)
  }

  def union [T]: Adder[T] = new Adder

  class Adder [T] {
    def apply [F[_], A1, A2, R <: Coproduct] (f1: => F[A1], f2: => F[A2]) (implicit aux1: Generic.Aux[T, R], eq: (A1 :+: A2 :+: CNil) =+= R, F: InvariantSemiringal[F]): F[T] = {
      F.select(f1, f2)
    }
  }
}

object InvariantSemiringalSyntax extends InvariantSemiringalSyntax
