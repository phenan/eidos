package com.phenan.eidos

import data._
import common.converter._
import syntax.converter._

import org.scalatest._

import scala.language.higherKinds

class ConverterTest extends FunSuite with DiagrammedAssertions {

  test ("integer") {
    assert(integer[PEGParser[Char, ?]].evalParser("120".toStream).contains(120))
    assert(integer[StreamPrinter[Char, ?]].runPrinter(567) == "567".toStream)
  }

  test ("java identifier") {
    assert(javaIdentifier[PEGParser[Char, ?]].evalParser("JavaIdentifier".toStream).contains("JavaIdentifier"))
    assert(javaIdentifier[StreamPrinter[Char, ?]].runPrinter("JavaIdentifier") == "JavaIdentifier".toStream)
  }

  test ("keyword") {
    assert(keyword[PEGParser[Char, ?]]("hoge").runParser("hogehuga".toStream).contains(("huga".toStream, ())))
    assert(keyword[StreamPrinter[Char, ?]]("hoge").runPrinter(()) == "hoge".toStream)
  }

  test ("hoge") {
    assert(Test.hoge[PEGParser[Char, ?]].evalParser("bar: 123, hoge".toStream).contains(Bar(123, "hoge")))
    assert(Test.hoge[StreamPrinter[Char, ?]].runPrinter(Bar(-456, "piyo")) == "bar: -456, piyo".toStream)
  }
}

sealed trait Hoge
case object Foo extends Hoge
case class Bar (n: Int, s: String) extends Hoge

object Test {
  def hoge [F[_]] (implicit F: Converter[F, Char]): F[Hoge] = union[Hoge] (
    keyword("foo") *> struct[Foo.type](),
    keyword("bar: ") *> struct[Bar](integer, keyword(", ") *> javaIdentifier)
  )
}
