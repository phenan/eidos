package com.phenan.eidos.data

case class NonEmptyList [+T] (head: T, tail: List[T]) {

  def last: T = tail.lastOption.getOrElse(head)
  def init: List[T] = tail match {
    case Nil    => Nil
    case nonNil => head :: nonNil.init
  }

  def map [U] (f: T => U): NonEmptyList[U] = NonEmptyList(f(head), tail.map(f))

  def fold [A >: T] (z: A)(f: (A, A) => A): A = tail.fold(f(z, head))(f)
  def foldLeft [A] (z: A)(f: (A, T) => A): A = tail.foldLeft(f(z, head))(f)
  def foldRight [A] (z: A)(f: (T, A) => A): A = f(head, tail.foldRight(z)(f))

  def reduce [A >: T] (f: (A, A) => A): A = reduceLeft(f)
  def reduceLeft [A >: T] (f: (A, T) => A): A = tail.foldLeft[A](head)(f)
  def reduceRight [A >: T] (f: (T, A) => A): A = (head :: tail).reduceRight(f)

  def mkString: String = (head :: tail).mkString
  def mkString (sep: String): String = (head :: tail).mkString(sep)
  def mkString (start: String, sep: String, end: String): String = (head :: tail).mkString(start, sep, end)
}
