import cats.Monoid
import cats.syntax.monoid._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// Implement a parallel version of foldMap called parallelFoldMap.
// Here is the type signature:

def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
  val processors = Runtime.getRuntime.availableProcessors

  val batch = values
    .grouped(values.length / processors)
    .map(list =>
      Future {
        list.map(f)
          .foldLeft(Monoid[B].empty)(_ |+| _)
      }
    )

  Future.sequence(batch)
        .map(_.foldLeft(Monoid[B].empty)(_ |+| _))
}

import cats.instances.string._

var result: Future[String] = parallelFoldMap(Vector(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0))(_.toString)
Await.result(result, 1 second)

// For bonus points, process the batches for each CPU using your implementation of foldMap from above.

def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B = {
  seq
    .map(f)
    .foldLeft(Monoid[B].empty)(_ |+| _)
}

def parallelUsingFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
  val processors = Runtime.getRuntime.availableProcessors

  val batch = values
    .grouped(values.length / processors)
    .map(x =>
      Future {
        foldMap(x)(f)
      }
    )

  Future.sequence(batch)
    .map(_.foldLeft(Monoid[B].empty)(_ |+| _))
}

result = parallelUsingFoldMap(Vector(1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0))(_.toString)
Await.result(result, 1 second)

import cats.instances.int._

val result2 = parallelUsingFoldMap((1 to 999999).toVector)(identity)
Await.result(result2, 1 second)
