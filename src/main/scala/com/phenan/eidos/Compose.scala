package com.phenan.eidos

import scala.language.higherKinds

trait Compose [=>:[_, _]] {
  def compose [A, B, C] (f: A =>: B)(g: B =>: C): A =>: C
}

