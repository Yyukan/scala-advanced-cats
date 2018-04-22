/**
 Although we implemented foldMap ourselves above, the method is also avail-
 able as part of the Foldable type class. Re-implement parallelFoldMap using Cats’ Foldable and Traverseable
 type classes.
 */

import cats.{Foldable, Monoid, Traverse}
import cats.syntax.monoid._
import cats.instances.vector._
import cats.instances.future._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
  val processors = Runtime.getRuntime.availableProcessors

  val batch = values
    .grouped(values.length / processors)
    .map(list =>
      Future {
        Foldable[Vector].foldLeft(list.map(f), Monoid[B].empty)(_ |+| _)
      }
    )
    .toVector

  Traverse[Vector]
    .sequence(batch)
    .map(_.foldLeft(Monoid[B].empty)(_ |+| _))
}

import cats.instances.string._

var result: Future[String] = parallelFoldMap(Vector(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0))(_.toString)
Await.result(result, 1 second)

import cats.instances.int._

val result2: Future[Int] = parallelFoldMap((1 to 999999).toVector)(identity)
Await.result(result2, 1 second)
