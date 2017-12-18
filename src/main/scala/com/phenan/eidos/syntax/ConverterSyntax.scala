package com.phenan.eidos
package syntax

import scala.language.higherKinds

trait ConverterSyntax {
  def satisfy [F[_], E] (f: E => Boolean) (implicit F: Converter[F, E]): F[E] = F.satisfy(f)

  def element [F[_], E] (e: E)(implicit F: Converter[F, E]): F[Unit] = F.element(e)
}

object ConverterSyntax extends ConverterSyntax