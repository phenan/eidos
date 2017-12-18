package com.phenan.eidos
package common

import data._
import syntax.converter._
import util.coproduct._

import shapeless._

import scala.language.higherKinds

trait CommonConverters {

  def digit [F[_]] (implicit F: Converter[F, Char]) : F[Char] = satisfy(Character.isDigit)

  def nonZeroDigit [F[_]] (implicit F: Converter[F, Char]) : F[Char] = satisfy(c => Character.isDigit(c) && c != '0')

  def naturalNumber [F[_]] (implicit F: Converter[F, Char]) : F[Int] = {
    naturalNumberDigits.xmap(_.foldLeft(0)(_ * 10 + _))(naturalNumberToNel(_, Nil))
  }

  def naturalNumberDigits [F[_]] (implicit F: Converter[F, Char]) : F[NonEmptyList[Int]] = {
    struct[NonEmptyList[Char]](nonZeroDigit, digit.rep)
      .xmap(_.map(Character.digit(_, 10)))(_.map(Character.forDigit(_, 10)))
  }

  def naturalNumberToNel (n: Int, list: List[Int]): NonEmptyList[Int] = {
    require(n > 0)
    if (n > 10) naturalNumberToNel(n / 10, n % 10 :: list)
    else NonEmptyList(n, list)
  }

  def negativeInteger [F[_]] (implicit F: Converter[F, Char]): F[Int] = ( element('-') *> naturalNumber ).xmap(-_)(_.abs)

  def integer [F[_]] (implicit F: Converter[F, Char]): F[Int] = {
    branch(naturalNumber, element('0'), negativeInteger)
      .xmap(_.foldN(identity, _ => 0, identity))(split3[Int](_ > 0, _ == 0)(_).map2(_ => ()))
  }

  def javaIdentifierStart [F[_]] (implicit F: Converter[F, Char]): F[Char] = satisfy(Character.isJavaIdentifierStart)

  def javaIdentifierPart [F[_]] (implicit F: Converter[F, Char]): F[Char] = satisfy(Character.isJavaIdentifierPart)

  def javaIdentifier [F[_]] (implicit F: Converter[F, Char]): F[String] = {
    struct[NonEmptyList[Char]](javaIdentifierStart, javaIdentifierPart.rep)
      .xmap(_.mkString)(s => NonEmptyList(s.head, s.tail.toList))
  }

  def whitespace [F[_]] (implicit F: Converter[F, Char]): F[Char] = satisfy(Character.isWhitespace)

  def keyword [F[_]] (s: String)(implicit F: Converter[F, Char]): F[Unit] = {
    s.toList.map(element(_)).foldRight(nil.discard(HNil))(_ *> _)
  }
}
