package com.phenan.eidos

import shapeless._

import scala.language.higherKinds

import util.coproduct._

trait InvariantArrow [:<=>:[_, _]] extends Category[:<=>:] with ComposeProduct[:<=>:] with InvariantStrong[:<=>:] {

  def lift [A, B] (f: A => B)(g: B => A): A :<=>: B

  override def id[A]: A :<=>: A = lift(identity[A])(identity[A])

  override def first [A, B, C <: HList] (f: A :<=>: B): (A :: C) :<=>: (B :: C) = product(f)(id)

  override def second[A <: HList, B <: HList, C] (f: A :<=>: B): (C :: A) :<=>: (C :: B) = product(id[C])(f)

  override def dimap [A, B, C, D] (f: A :<=>: B)(g: A => C)(h: C => A)(i: B => D)(j: D => B): C :<=>: D = {
    compose(compose(lift(h)(g))(f))(lift(i)(j))
  }

  def singleElemHList [A]: (A :: HNil) :<=>: A = lift[A :: HNil, A](_.head)(_ :: HNil)
  def singleElemCoproduct [A]: (A :+: CNil) :<=>: A = lift[A :+: CNil, A](_.get)(Inl(_))

  def construct [T]: Constructor[T] = new Constructor[T]
  def destruct [T]: Destructor[T] = new Destructor[T]

  class Constructor [T] {
    def apply [R] (implicit aux: Generic.Aux[T, R]): R :<=>: T = lift(aux.from)(aux.to)
  }

  class Destructor [T] {
    def apply [R] (implicit aux: Generic.Aux[T, R]): T :<=>: R = lift(aux.to)(aux.from)
  }

}
