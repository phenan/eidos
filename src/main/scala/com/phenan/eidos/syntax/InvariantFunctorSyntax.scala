package com.phenan.eidos
package syntax

import shapeless._

import scala.language.higherKinds

trait InvariantFunctorSyntax {
  implicit class InvariantFunctorOps [F[_], A] (fa: => F[A])(implicit F: InvariantFunctor[F]) {
    def xmap [B] (f: A => B)(g: B => A): F[B] = F.xmap(f)(g)(fa)
    def discard (a: => A): F[Unit] = F.discard(a)(fa)
    def wrap [B] (implicit aux: Generic.Aux[B, A]): F[B] = F.wraps[B, A](fa)
  }
}

object InvariantFunctorSyntax extends InvariantFunctorSyntax
