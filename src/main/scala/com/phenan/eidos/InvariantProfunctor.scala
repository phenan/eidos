package com.phenan.eidos

import scala.language.higherKinds

trait InvariantProfunctor [:<=>:[_, _]] {
  def dimap [A, B, C, D] (f: A :<=>: B)(g: A => C)(h: C => A)(i: B => D)(j: D => B): C :<=>: D
}
