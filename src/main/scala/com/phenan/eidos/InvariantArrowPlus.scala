package com.phenan.eidos

import scala.language.higherKinds

trait InvariantArrowPlus [:<=>:[_, _]] extends InvariantArrow[:<=>:] {
  def plus [A, B] (f: A :<=>: B)(g: A :<=>: B): A :<=>: B
}
