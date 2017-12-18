package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait InvariantStrong [:<=>:[_, _]] extends InvariantProfunctor[:<=>:] {
  def first [A, B, C <: HList] (f: A :<=>: B): (A :: C) :<=>: (B :: C)

  def second [A <: HList, B <: HList, C] (f: A :<=>: B): (C :: A) :<=>: (C :: B) = {
    def g [X <: HList, Y] : X :: Y :: HNil => Y :: X = { case x :: y :: HNil => y :: x }
    def h [X <: HList, Y] : Y :: X => X :: Y :: HNil = { case y :: x => x :: y :: HNil }

    dimap[A :: C :: HNil, B :: C :: HNil, C :: A, C :: B] (first(f))(g)(h)(g)(h)
  }
}
