package io.lbert.ml.fp

import language.higherKinds
import scala.util.Try

trait Functor[F[_]] { self =>

  def map[A,B](fa: F[A])(f: A => B): F[B]

  def lift[A,B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)
  //This shit is crazy...
  def compose[G[_]: Functor]: Functor[({type f[x] = F[G[x]]})#f] = {
    val gf = implicitly[Functor[G]]
    new Functor[({type f[x] = F[G[x]]})#f] {
      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
        self.map(fa)(ff => gf.map(ff)(f))
    }
  }
}

object Functor {

  def apply[F[_]](implicit f: Functor[F]): Functor[F] = f

}

trait Applicative[F[_]] extends Functor[F] {

  def ap[A,B](fab: F[A => B])(f: F[A]): F[B]

  def pure[A](a: => A): F[A]

}

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    flatMap(fab)(f => map(fa)(a => f(a)))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

}

object Monad {

  def apply[F[_]](implicit m: Monad[F]): Monad[F] = m

}

trait Semigroup[A] {

  def append(a1: A, a2: A): A

}

trait Monoid[A] extends Semigroup[A] {

  def zero: A

}

object MonadInstances {

  implicit val optionMonad = new Monad[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B) = fa.map(f)
    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]) = fa.flatMap(f)
    override def pure[A](a: => A) = Some(a)
  }

  implicit val listMonad = new Monad[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
    override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa.flatMap(f)
    override def pure[A](a: => A): List[A] = List(a)
  }

  implicit def eitherMonad[E]: Monad[({ type L[x] = Either[E, x] })#L] =
    new Monad[({ type L[x] = Either[E, x] })#L] {
      def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.map(f)
      override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]) = fa.flatMap(f)
      override def pure[A](a: => A) = Right(a)
    }

  implicit val ioMonad = new Monad[IO] {
    override def map[A, B](fa: IO[A])(f: (A) => B): IO[B] = fa.map(f)
    override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa.flatMap(f)
    override def pure[A](a: => A): IO[A] = IO(a)
  }

  implicit def tryMonad = new Monad[Try] {
    override def map[A, B](fa: Try[A])(f: (A) => B) = fa.map(f)
    override def flatMap[A, B](fa: Try[A])(f: (A) => Try[B]) = fa.flatMap(f)
    override def pure[A](a: => A) = Try(a)
  }
}