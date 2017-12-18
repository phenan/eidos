package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

trait InvariantArrowApply [:<=>:[_, _]] extends InvariantArrow [:<=>:] {
  def app [A, B, C <: HList]: ((A :<=>: B) :: A :: C) :<=>: ((A :<=>: B) :: B :: C)

  def applyUnit [A]: (Unit :<=>: A) :<=>: A = {
    val f: (Unit :<=>: A) :<=>: ((Unit :<=>: A) :: A :: HNil) = {
      compose(lift[Unit :<=>: A, (Unit :<=>: A) :: Unit :: HNil](_ :: () :: HNil)(_.head))(app)
    }
    val g: ((Unit :<=>: A) :: A :: HNil) :<=>: A = {
      lift[(Unit :<=>: A) :: A :: HNil, A](_.tail.head)(a => lift[Unit, A](_ => a)(_ => ()) :: a :: HNil)
    }
    compose(f)(g)
  }
}
