package com.phenan.eidos.util

import shapeless._

package object coproduct {
  implicit class CoproductOps [X, Y, Z <: Coproduct] (c: X :+: Y :+: Z) {
    def swap: Y :+: X :+: Z = c match {
      case Inl(x)      => Inr(Inl(x))
      case Inr(Inl(y)) => Inl(y)
      case Inr(Inr(z)) => Inr(Inr(z))
    }
  }

  implicit class Coproduct1Ops [X] (c: X :+: CNil) {
    def get: X = c match {
      case Inl(x) => x
      case Inr(r) => r.impossible
    }
    def foldN [R] (f: X => R): R = f(get)
    def map [Y] (f: X => Y): Y:+: CNil = c match {
      case Inl(x) => Inl(f(x))
      case Inr(r) => Inr(r)
    }
  }

  implicit class Coproduct2Ops [X1, X2] (c: X1 :+: X2 :+: CNil) {
    def foldN [R] (f1: X1 => R, f2: X2 => R): R = c match {
      case Inl(x) => f1(x)
      case Inr(r) => r.foldN(f2)
    }
    def mapAll [Y1, Y2] (f1: X1 => Y1, f2: X2 => Y2): Y1 :+: Y2 :+: CNil = c match {
      case Inl(x) => Inl(f1(x))
      case Inr(r) => Inr(r.map(f2))
    }
    def map1 [Y] (f: X1 => Y): Y :+: X2 :+: CNil = c match {
      case Inl(x) => Inl(f(x))
      case Inr(r) => Inr(r)
    }
    def map2 [Y] (f: X2 => Y): X1 :+: Y :+: CNil = c match {
      case Inl(x) => Inl(x)
      case Inr(r) => Inr(r.map(f))
    }
  }

  implicit class Coproduct3Ops [X1, X2, X3] (c: X1 :+: X2 :+: X3 :+: CNil) {
    def foldN [R] (f1: X1 => R, f2: X2 => R, f3: X3 => R): R = c match {
      case Inl(x) => f1(x)
      case Inr(r) => r.foldN(f2, f3)
    }
    def mapAll [Y1, Y2, Y3] (f1: X1 => Y1, f2: X2 => Y2, f3: X3 => Y3): Y1 :+: Y2 :+: Y3 :+: CNil = c match {
      case Inl(x) => Inl(f1(x))
      case Inr(r) => Inr(r.mapAll(f2, f3))
    }
    def map1 [Y] (f: X1 => Y): Y :+: X2 :+: X3 :+: CNil = c match {
      case Inl(x) => Inl(f(x))
      case Inr(r) => Inr(r)
    }
    def map2 [Y] (f: X2 => Y): X1 :+: Y :+: X3 :+: CNil = c match {
      case Inl(x) => Inl(x)
      case Inr(r) => Inr(r.map1(f))
    }
    def map3 [Y] (f: X3 => Y): X1 :+: X2 :+: Y :+: CNil = c match {
      case Inl(x) => Inl(x)
      case Inr(r) => Inr(r.map2(f))
    }
  }

  implicit class CoproductFunctionOps [X <: Coproduct, R] (f: X => R) {
    def :|: [Y] (g: Y => R): Y :+: X => R = {
      case Inl(y) => g(y)
      case Inr(x) => f(x)
    }
  }

  def absurd [R] : CNil => R = _.impossible

  def split2 [R] (f: R => Boolean): R => R :+: R :+: CNil = r => {
    if (f(r)) Inl(r)
    else Inr(Inl(r))
  }

  def split3 [R] (f1: R => Boolean, f2: R => Boolean): R => R :+: R :+: R :+: CNil = r => {
    if (f1(r)) Inl(r)
    else Inr(split2(f2)(r))
  }

  type =+= [A <: Coproduct, B <: Coproduct] = (A IsSubCoproductOf B, B IsSubCoproductOf A)

  implicit def equivalentCoproduct [A <: Coproduct, B <: Coproduct] (implicit f: A IsSubCoproductOf B, g: B IsSubCoproductOf A): A =+= B = (f, g)
}
