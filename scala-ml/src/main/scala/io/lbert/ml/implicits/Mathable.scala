package io.lbert.ml.implicits

trait Mathable[A] {
  def add(a1: A, a2: A): A
  def subtract(a1: A, a2: A): A
  def multiply(a1: A, a2: A): A
  def divide(a1: A, a2: A): A
  def lessThan(a1: A, a2: A): Boolean
  def zero: A
  def one: A
  def toInt(a: A): Int
  def negOne: A
}

object Mathable {
  def apply[A](
    _add: (A,A) => A,
    _subtract: (A,A) => A,
    _multiply: (A,A) => A,
    _divide: (A,A) => A,
    _lessThan: (A,A) => Boolean,
    _zero: A,
    _one: A,
    _toInt: A => Int,
    _negOne: A
  ): Mathable[A] = new Mathable[A] {
    override def divide(a1: A, a2: A): A = _divide(a1,a2)
    override def multiply(a1: A, a2: A): A = _multiply(a1,a2)
    override def subtract(a1: A, a2: A): A = _subtract(a1,a2)
    override def add(a1: A, a2: A): A = _add(a1,a2)
    override def lessThan(a1: A, a2: A): Boolean = _lessThan(a1,a2)
    override def zero: A = _zero
    override def one: A = _one
    override def toInt(a: A): Int = _toInt(a)
    override def negOne: A = _negOne
  }
}

object MathableInstances {
  implicit val intMathable: Mathable[Int] = Mathable[Int](
    _ + _, _ - _, _ * _, _ / _, _ < _, 0, 1, identity, -1
  )

  implicit val doubleMathable: Mathable[Double] = Mathable[Double](
    _ + _, _ - _, _ * _, _ / _, _ < _, 0, 1, _.toInt, -1
  )

  implicit val longMathable: Mathable[Long] = Mathable[Long](
    _ + _, _ - _, _ * _, _ / _, _ < _, 0, 1, _.toInt, -1
  )
}

object MathableSyntax {
  implicit class MathableOps[A](a: A) {
    def +(oa: A)(implicit mathable: Mathable[A]): A =
      mathable.add(a,oa)
    def -(oa: A)(implicit mathable: Mathable[A]): A =
      mathable.subtract(a,oa)
    def *(oa: A)(implicit mathable: Mathable[A]): A =
      mathable.multiply(a,oa)
    def /(oa: A)(implicit mathable: Mathable[A]): A =
      mathable.divide(a,oa)

    def <(oa: A)(implicit mathable: Mathable[A]): Boolean =
      mathable.lessThan(a, oa)

    def toInt(implicit mathable: Mathable[A]): Int =
      mathable.toInt(a)
  }
}