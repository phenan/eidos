package com.phenan.eidos
package syntax

import shapeless._

import scala.language.higherKinds

trait MonoidalSyntax {
  implicit class MonoidalOps [F[_] : Monoidal, A <: HList] (fa: => F[A]) {
    def *: [B] (fb: F[B]): F[B :: A] = implicitly[Monoidal[F]].product(fb)(fa)
  }
  def nil [F[_]] (implicit F: Monoidal[F]): F[HNil] = F.nil
}

object MonoidalSyntax extends MonoidalSyntax
