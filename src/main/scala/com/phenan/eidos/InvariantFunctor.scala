package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait InvariantFunctor [F[_]] {
  def xmap [A, B]: (A => B) => (B => A) => F[A] => F[B]

  def discard [A]: A => F[A] => F[Unit] = a => fa => xmap[A, Unit](_ => ())(_ => a)(fa)

  def wraps [A, R] (repr: F[R])(implicit aux: Generic.Aux[A, R]): F[A] = xmap[R, A](aux.from)(aux.to)(repr)
}
