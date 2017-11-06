package io.lbert.ml

import cats.implicits._

case class Row(underlying: Int) extends AnyVal
case class Column(underlying: Int) extends AnyVal

case class MatrixIndex(row: Row, column: Column)
case class MatrixSize(rows: Row, columns: Column) {
  override def toString: String = s"${rows.underlying}x${columns.underlying}"
}
case class VectorIndex(row: Row)
case class VectorSize(rows: Row)

case class Matrix[A](elements: Seq[Seq[A]])

object Matrix {

  type MatrixResult[A] = Either[String, Matrix[A]]

  def add(matrix1: Matrix[Int], matrix2: Matrix[Int]): MatrixResult[Int] = {
    val matrix1Size = size(matrix1)
    val matrix2Size = size(matrix2)
    if(matrix1Size != matrix2Size) {
      Left(s"Matrices are not the same size. $matrix1Size and $matrix2Size are not equal")
    } else {
      Right(Matrix(getIndexes(matrix1Size).flatMap {index =>
        for {
          i1 <- get(matrix1, index)
          i2 <- get(matrix2, index)
        } yield i1 + i2
      }.grouped(matrix1Size.columns.underlying).toSeq))
    }
  }

  def get[A](matrix: Matrix[A], index: MatrixIndex): Option[A] =
    if(index.row.underlying > rows(matrix).underlying || index.column.underlying > columns(matrix).underlying) {
      None
    } else {
      Some(matrix.elements(index.row.underlying - 1)(index.column.underlying - 1))
    }

  def getIndexes(size: MatrixSize): Seq[MatrixIndex] =
    for {
      row    <- 1 to size.rows.underlying
      column <- 1 to size.columns.underlying
    } yield MatrixIndex(Row(row),Column(column))

  def getIndexes(matrix: Matrix[_]): Seq[MatrixIndex] = getIndexes(size(matrix))

  def dimension(matrix: Matrix[_]): Int = {
    val s = size(matrix)
    s.rows.underlying * s.columns.underlying
  }

  def size(matrix: Matrix[_]): MatrixSize =
    MatrixSize(rows(matrix), columns(matrix))

  def rows(matrix: Matrix[_]): Row =
    Row(matrix.elements.size)

  def columns(matrix: Matrix[_]): Column =
    Column(matrix.elements.headOption.map(_.size).getOrElse(0))

  def fillFunc[A](size: MatrixSize, fillFunc: MatrixIndex => A): Matrix[A] =
    Matrix((1 to size.rows.underlying).map(row =>
      (1 to size.columns.underlying).map(column =>
        fillFunc(MatrixIndex(Row(row),Column(column)))
      )
    ))

  def fill[A](size: MatrixSize, value: A): Matrix[A] =
    fillFunc(size,_ => value)

  def empty[A]: Matrix[A] = Matrix[A](Seq.empty[Seq[A]])
}

object Vector {

  def empty[A]: Matrix[A] = Matrix.empty[A]

  def apply[A](elements: Seq[A]): Matrix[A] =
    Matrix(elements.map(Seq(_)))

  def fill[A](size: VectorSize, value: A): Matrix[A] =
    Matrix.fill[A](MatrixSize(size.rows,Column(1)),value)

  def fillFunc[A](size: VectorSize, fillFunc: VectorIndex => A): Matrix[A] =
    Matrix.fillFunc[A](MatrixSize(size.rows,Column(1)),mi => fillFunc(VectorIndex(mi.row)))
}
