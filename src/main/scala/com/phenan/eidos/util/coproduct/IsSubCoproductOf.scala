package com.phenan.eidos.util.coproduct

import shapeless._
import shapeless.ops.coproduct.Inject

case class IsSubCoproductOf [A <: Coproduct, B <: Coproduct] (translate: A => B)

object IsSubCoproductOf {
  implicit def simpleTranslation [A, B <: Coproduct] (implicit inject: Inject[B, A]): (A :+: CNil) IsSubCoproductOf B = IsSubCoproductOf {
    a => inject(a.eliminate(x => x, _.impossible))
  }
  implicit def composedTranslation [A, B <: Coproduct, C <: Coproduct] (implicit inject: Inject[C, A], translation: B IsSubCoproductOf C): (A :+: B) IsSubCoproductOf C = IsSubCoproductOf {
    case Inl(a) => inject(a)
    case Inr(b) => translation.translate(b)
  }
}
