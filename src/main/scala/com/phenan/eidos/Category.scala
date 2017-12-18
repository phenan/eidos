package com.phenan.eidos

import scala.language.higherKinds

trait Category [=>:[_, _]] extends Compose[=>:] {
  def id[A]: A =>: A
}
