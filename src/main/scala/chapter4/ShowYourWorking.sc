/**
 Writers are useful for logging opera ons in multithreaded environments.
 Let’s confirm this by computing (and logging) some factorials.
 */

def slowly[A](body: => A) =
  try body finally Thread.sleep(100)

def factorial(n: Int): Int = {
  val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
  println(s"fact $n $ans")
  ans
}

factorial(5)

// If we start several factorials in parallel,
// the log messages can become inter- leaved on standard out.
// This makes it difficult to see which messages come from which computation

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

Await.result(Future.sequence(Vector(
  Future(factorial(3)),
  Future(factorial(3))
)), 5.seconds)

// Rewrite factorial so it captures the log messages in a Writer.
// Demonstrate that this allows us to reliably separate the logs for concurrent computations

import cats.data.Writer
import cats.syntax.applicative._
import cats.instances.vector._

type Logged[A] = Writer[Vector[String], A]

def factorialWriter(n: Int): Logged[Int] = {
  val result = slowly(if(n == 0) 1.pure[Logged] else factorialWriter(n - 1).map(_ * n))

  result.mapWritten(_ :+ s"fact $n ${result.value}")
}

factorialWriter(5).run


