package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

import util.coproduct._

trait InvariantChoice [:<=>:[_, _]] extends InvariantProfunctor[:<=>:] {
  def left [A, B, C <: Coproduct] (f: A :<=>: B): (A :+: C) :<=>: (B :+: C)

  def right [A <: Coproduct, B <: Coproduct, C] (f: A :<=>: B): (C :+: A) :<=>: (C :+: B) = {
    def g [X <: Coproduct, Y] : X :+: Y :+: CNil => Y :+: X = _.foldN(Inr(_), Inl(_))
    def h [X <: Coproduct, Y] : Y :+: X => X :+: Y :+: CNil = {
      case Inl(y) => Inr(Inl(y))
      case Inr(x) => Inl(x)
    }
    dimap[A :+: C :+: CNil, B :+: C :+: CNil, C :+: A, C :+: B](left(f))(g)(h)(g)(h)
  }
}
