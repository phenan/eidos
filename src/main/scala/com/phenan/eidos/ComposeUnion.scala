package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait ComposeUnion [=>:[_, _]] extends Compose [=>:] {
  def coproduct [A, B, C <: Coproduct, D <: Coproduct] (f: A =>: B)(g: C =>: D): (A :+: C) =>: (B :+: D)

  def coproduct3 [A1, A2, A3 <: Coproduct, B1, B2, B3 <: Coproduct] (f1: A1 =>: B1)(f2: A2 =>: B2)(f3: A3 =>: B3): (A1 :+: A2 :+: A3) =>: (B1 :+: B2 :+: B3) = {
    coproduct(f1)(coproduct(f2)(f3))
  }
}
