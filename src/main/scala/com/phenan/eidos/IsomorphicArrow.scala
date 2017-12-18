package com.phenan.eidos

import scala.language.higherKinds

trait IsomorphicArrow [:<=>:[_, _]] {
  def iso[A, B]: (A :<=>: B) :<=>: (B :<=>: A)
}
