package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait InvariantArrowChoice [:<=>:[_, _]] extends InvariantArrow[:<=>:] with ComposeUnion[:<=>:] with InvariantChoice[:<=>:] {

  override def left [A, B, C <: Coproduct] (f: A :<=>: B): (A :+: C) :<=>: (B :+: C) = coproduct(f)(id)

  override def right [A <: Coproduct, B <: Coproduct, C] (f: A :<=>: B): (C :+: A) :<=>: (C :+: B) = coproduct(id[C])(f)

}
