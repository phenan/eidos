package com.phenan.eidos
package data

import shapeless._

case class PEGParser [E, T] (runParser: Stream[E] => Option[(Stream[E], T)]) {
  def evalParser (s: Stream[E]): Option[T] = runParser(s).map(_._2)
}

object PEGParser {

  implicit def PEGParserConverter[E]: Converter[PEGParser[E, ?], E] = new Converter[PEGParser[E, ?], E] {

    override def zero: PEGParser[E, CNil] = PEGParser(_ => None)

    override def pure[A] (a: => A): PEGParser[E, A] = PEGParser(s => Some((s, a)))

    override def satisfy: (E => Boolean) => PEGParser[E, E] = f => PEGParser {
      case h #:: t if f(h) => Some((t, h))
      case _               => None
    }

    override def xmap[A, B]: (A => B) => (B => A) => PEGParser[E, A] => PEGParser[E, B] = f => _ => p => PEGParser { s =>
      p.runParser(s).map { case (r, t) => (r, f(t)) }
    }

    override def product[A, B <: HList] (fa: => PEGParser[E, A])(fb: => PEGParser[E, B]): PEGParser[E, A :: B] = PEGParser { s0 =>
      for {
        (s1, a) <- fa.runParser(s0)
        (s2, b) <- fb.runParser(s1)
      } yield (s2, a :: b)
    }

    override def append[A, B <: Coproduct] (fa: => PEGParser[E, A])(fb: => PEGParser[E, B]): PEGParser[E, A :+: B] = PEGParser { s =>
      fa.runParser(s).map { case (r, a) => (r, Inl(a)) } orElse fb.runParser(s).map { case (r, b) => (r, Inr(b)) }
    }

  }
}
