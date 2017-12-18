package com.phenan.eidos

import scala.language.higherKinds

trait Converter [F[_], E] extends InvariantSemiringal[F] {

  import syntax.invariantSemiringal._
  private implicit def self: InvariantSemiringal[F] = this

  def satisfy: (E => Boolean) => F[E]

  def element (e: E): F[Unit] = satisfy(e == _).discard(e)

}
