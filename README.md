# eidos
Invariants functors and arrows with generics in shapeless

## Converter

`Converter` is a type class that represents both of parsers and printers.
We can construct parsers and printers by a single definition.

The following definition is a sample program using `Converter`.

```scala
import com.phenan.eidos._
import com.phenan.eidos.common.converter._
import com.phenan.eidos.syntax.converter._
import scala.language.higherKinds

sealed trait Hoge
case object Foo extends Hoge
case class Bar (n: Int, s: String) extends Hoge

object Test {
  def hoge [F[_]] (implicit F: Converter[F, Char]): F[Hoge] = union[Hoge] (
    keyword("foo")   *> struct[Foo.type](),
    keyword("bar: ") *> struct[Bar](integer, keyword(", ") *> javaIdentifier)
  )
}
```

`union` and `struct` is a combinator of `Converter` using generics in shapeless.
They express composition of `Converter`s following to algebraic data types.
`union` is a parallel composition of `Converter`s and it returns a `Converter` of the super type that is defined as `sealed class` or `sealed trait`.
`union` takes a type that is the super type and it takes `Converter`s for all the sub types as arguments.
It returns the `Converter` of the given super type.
Surprisingly, we do not need to write a conversion between the super type (`Hoge` in this example) and the sub types (`Foo` and `Bar`).
Such conversion is automatically inferred by generics.

`struct` is a sequential composition of `Converter`s and it returns a `Converter` of the composed data type that is defined as `case class`.
`struct` takes a type that is the composed data type and it takes `Converter`s for all argument types of the primary constructor of the data.
For example, `Bar` takes two arguments, `Int` and `String`, 
so `struct[Bar]` takes two `Converter`s of `Int` and `String`.

Converter is implemented as a tagless-final style, 
so we should declare which interpreter we want to use.
This library provides `PEGParser` and `StreamPrinter` as an interpreter of `Converter`.
The following program is a sample program that interprets `Test.hoge`.

```scala
 import com.phenan.eidos.data._
 
 val x = Test.hoge[PEGParser[Char, ?]].evalParser("bar: 123, hoge".toStream)
 // x : Some(Bar(123, "hoge"))
 
 val y = Test.hoge[StreamPrinter[Char, ?]].runPrinter(Bar(-456, "piyo"))
 // y : "bar: -456, piyo".toStream
```

## Author

[@phenan](https://twitter.com/phenan)