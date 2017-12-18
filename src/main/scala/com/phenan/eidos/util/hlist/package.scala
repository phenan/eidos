package com.phenan.eidos.util

import shapeless._

package object hlist {
  implicit class Function2Util [T1, T2, R] (f: (T1, T2) => R) {
    def hlisted: T1 :: T2 :: HNil => R = {
      case t1 :: t2 :: HNil => f(t1, t2)
    }
  }

  implicit class Tuple2Util [T1, T2, R] (t: (T1, T2)) {
    def hlisted: T1 :: T2 :: HNil = t._1 :: t._2 :: HNil
  }
}
