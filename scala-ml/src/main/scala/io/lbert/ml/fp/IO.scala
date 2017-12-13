package io.lbert.ml.fp

import io.lbert.ml.ErrorOr

final class IO[A](val unsafePerformIO: () => A) {

  def map[B](ab: A => B): IO[B] =
    new IO(() => ab(unsafePerformIO()))

  def flatMap[B](afb: A => IO[B]): IO[B] =
    new IO(() => afb(unsafePerformIO()).unsafePerformIO())

  def attempt: IO[ErrorOr[A]] = new IO(() => {
    try Right(unsafePerformIO())
    catch {
      case t : Throwable => Left(t)
    }
  })
}

object IO {
  final def apply[A](a: => A): IO[A] = new IO(() => a)

  final def fail[A](t: Throwable): IO[A] = new IO(() => throw t)
}

final case class Async[A](register: (ErrorOr[A] => IO[Unit]) => IO[Unit]) { self =>
  def map[B](ab: A => B): Async[B] = Async[B] { callback: (ErrorOr[B] => IO[Unit]) =>
    self.register {
      case Left(e) => callback(Left(e))
      case Right(a) => callback(Right(ab(a)))
    }
  }

  def flatMap[B](afb: A => Async[B]): Async[B] = Async[B] { callback: (ErrorOr[B] => IO[Unit]) =>
    self.register {
      case Left(e) => callback(Left(e))
      case Right(a) => afb(a).register(callback)
    }
  }
}

object Async {
  final def apply[A](a: => A): Async[A] = Async[A] { callback: (ErrorOr[A] => IO[Unit]) =>
    callback(Right(a))
  }

  final def fail[A](e: Throwable): Async[A] = Async[A] { callback: (ErrorOr[A] => IO[Unit]) =>
    callback(Left(e))
  }
}