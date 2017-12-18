package com.phenan.eidos

import data._
import common.converter._

import org.scalatest._

import scala.language.higherKinds

class ConverterTest extends FunSuite with DiagrammedAssertions {

  test ("integer") {
    assert(integer[PEGParser[Char, ?]].evalParser("120".toStream).contains(120))
    assert(integer[SimplePrinter[Char, ?]].runPrinter(567) == "567".toStream)
  }

  test ("keyword") {
    assert(keyword[PEGParser[Char, ?]]("hoge").runParser("hogehuga".toStream).contains(("huga".toStream, ())))
    assert(keyword[SimplePrinter[Char, ?]]("hoge").runPrinter(()) == "hoge".toStream)
  }

}
