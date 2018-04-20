/**
 Here’s an example that uses Options:
 */

import cats._
import cats.implicits._

import scala.language.higherKinds // for Applicative

def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (acc, item) =>
    (acc, func(item)).mapN(_ :+ _)
  }

def process(inputs: List[Int]) =
  listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

process(List(2, 4, 6))
//res0: Option[List[Int]] = Some(List(2, 4, 6))
process(List(1, 2, 3))
//res1: Option[List[Int]] = None
