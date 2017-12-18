package com.phenan.eidos
package syntax

import shapeless._

import scala.language.higherKinds

trait SemiringalSyntax {
  implicit class SemiringalOps [F[_] : Semiringal, A <: Coproduct] (fa: => F[A]) {
    def +: [B] (fb: F[B]): F[B :+: A] = implicitly[Semiringal[F]].append(fb)(fa)
  }

  def branch [F[_], A, B] (fa: => F[A], fb: => F[B]) (implicit F: Semiringal[F]): F[A :+: B :+: CNil] = F.branch(fa, fb)
  def branch [F[_], A, B, C] (fa: => F[A], fb: => F[B], fc: => F[C]) (implicit F: Semiringal[F]): F[A :+: B :+: C :+: CNil] = F.branch(fa, fb, fc)

  def szero [F[_]] (implicit F: Semiringal[F]): F[CNil] = F.zero
}

object SemiringalSyntax extends SemiringalSyntax
