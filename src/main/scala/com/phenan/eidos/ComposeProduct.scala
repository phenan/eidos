package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait ComposeProduct [=>:[_, _]] extends Compose [=>:] {
  def product [A, B, C <: HList, D <: HList] (f: A =>: B)(g: C =>: D): (A :: C) =>: (B :: D)

  def product3 [A1, A2, A3 <: HList, B1, B2, B3 <: HList] (f1: A1 =>: B1)(f2: A2 =>: B2)(f3: A3 =>: B3): (A1 :: A2 :: A3) =>: (B1 :: B2 :: B3) = {
    product(f1)(product(f2)(f3))
  }
}
