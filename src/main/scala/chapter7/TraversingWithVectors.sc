/**
 What is the result of the following?
 */
import cats._
import cats.implicits._

import scala.language.higherKinds // for Applicative

def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (acc, item) =>
    (acc, func(item)).mapN(_ :+ _)
  }

def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)

listSequence(List(Vector(1, 2), Vector(3, 4)))
// res0: scala.collection.immutable.Vector[List[Int]] = Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
// res1: scala.collection.immutable.Vector[List[Int]] = Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))