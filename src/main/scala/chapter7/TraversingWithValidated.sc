/**
  * Finally, here is an example that uses Validated:
  */

import cats._
import cats.data.Validated
import cats.implicits._

import scala.language.higherKinds // for Applicative

def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (acc, item) =>
    (acc, func(item)).mapN(_ :+ _)
  }

type ErrorsOr[A] = Validated[List[String], A]

def process(inputs: List[Int]): ErrorsOr[List[Int]] = listTraverse(inputs) { n =>
  if (n % 2 == 0) {
    Validated.valid(n)
  } else {
    Validated.invalid(List(s"$n is not even"))
  }
}


process(List(2, 4, 6))
// res0: ErrorsOr[List[Int]] = Valid(List(2, 4, 6))
process(List(1, 2, 3))
// res1: ErrorsOr[List[Int]] = Invalid(List(1 is not even, 3 is not even))