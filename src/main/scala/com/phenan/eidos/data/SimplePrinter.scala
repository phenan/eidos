package com.phenan.eidos
package data

import shapeless._

case class SimplePrinter [E, T] (runPrinter: T => Stream[E])

object SimplePrinter {
  implicit def SimplePrinterConverter[E]: Converter[SimplePrinter[E, ?], E] = new Converter[SimplePrinter[E, ?], E] {

    override def zero: SimplePrinter[E, CNil] = SimplePrinter(_ => Stream.empty)

    override def pure[A] (a: => A): SimplePrinter[E, A] = SimplePrinter(_ => Stream.empty)

    override def satisfy: (E => Boolean) => SimplePrinter[E, E] = _ => SimplePrinter(e => Stream(e))

    override def xmap[A, B]: (A => B) => (B => A) => SimplePrinter[E, A] => SimplePrinter[E, B] = {
      _ => f => p => SimplePrinter(b => p.runPrinter(f(b)))
    }

    override def product[A, B <: HList] (fa: => SimplePrinter[E, A])(fb: => SimplePrinter[E, B]): SimplePrinter[E, A :: B] = SimplePrinter {
      case a :: b => fa.runPrinter(a) ++ fb.runPrinter(b)
    }

    override def append[A, B <: Coproduct] (fa: => SimplePrinter[E, A])(fb: => SimplePrinter[E, B]): SimplePrinter[E, A :+: B] = SimplePrinter {
      case Inl(a) => fa.runPrinter(a)
      case Inr(b) => fb.runPrinter(b)
    }

  }
}
