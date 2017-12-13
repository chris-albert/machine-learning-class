package io.lbert

package object ml {
  type ErrorOr[A] = Either[Throwable, A]
}
