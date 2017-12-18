package com.phenan.eidos
package data

import shapeless._

case class StreamPrinter [E, T] (runPrinter: T => Stream[E])

object StreamPrinter {
  implicit def SimplePrinterConverter[E]: Converter[StreamPrinter[E, ?], E] = new Converter[StreamPrinter[E, ?], E] {

    override def zero: StreamPrinter[E, CNil] = StreamPrinter(_ => Stream.empty)

    override def pure[A] (a: => A): StreamPrinter[E, A] = StreamPrinter(_ => Stream.empty)

    override def satisfy: (E => Boolean) => StreamPrinter[E, E] = _ => StreamPrinter(e => Stream(e))

    override def xmap[A, B]: (A => B) => (B => A) => StreamPrinter[E, A] => StreamPrinter[E, B] = {
      _ => f => p => StreamPrinter(b => p.runPrinter(f(b)))
    }

    override def product[A, B <: HList] (fa: => StreamPrinter[E, A])(fb: => StreamPrinter[E, B]): StreamPrinter[E, A :: B] = StreamPrinter {
      case a :: b => fa.runPrinter(a) ++ fb.runPrinter(b)
    }

    override def append[A, B <: Coproduct] (fa: => StreamPrinter[E, A])(fb: => StreamPrinter[E, B]): StreamPrinter[E, A :+: B] = StreamPrinter {
      case Inl(a) => fa.runPrinter(a)
      case Inr(b) => fb.runPrinter(b)
    }

  }
}
