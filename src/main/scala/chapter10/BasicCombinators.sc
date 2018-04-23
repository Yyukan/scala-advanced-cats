/**
 The problem is: what do you do when both checks fail?
 The correct thing to do is to return both errors, but we don’t currently have any way to combine Es.
 We need a type class that abstracts over the concept of “accumulating” errors.
 What type class do we know that looks like this?
 What method or operator should we use to implement the • operation?  */
import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.int._

sealed trait Check[E, A] {

  def and(that: Check[E, A])(implicit e: Monoid[E], v: Monoid[A]): Check[E, A] = (this, that) match {
    case (Success(thisValue), Success(thatValue)) => Success(thisValue |+| thatValue)
    case (Success(_), Failure(_)) => that
    case (Failure(_), Success(_)) => this
    case (Failure(thisErrors), Failure(thatErrors)) => Failure(thisErrors |+| thatErrors)
  }
}

case class Success[E, A](value: A) extends Check[E, A] {
  def apply(value: A): Check[E, A] = Success(value)
}

case class Failure[E, A](errors: E) extends Check[E, A] {
  def apply(errors: E): Check[E, A] = Failure(errors)
}


Success(5).and(Success(10))
//Failure(List("error1")).and(Success(5))
//Success(5).and(Failure(List("error2")))
//Failure(List("error1")).and(Failure(List("error2")))

// There is another semantic issue that will come up quite quickly: should and short-circuit if the first check fails.
// What do you think the most useful behaviour is?

// Use this knowledge to implement and. Make sure you end up with the behaviour you expect!

// Strictly speaking, Either[E, A] is the wrong abstraction for the output of our check. Why is this the case?
// What other data type could we use instead? Switch your implementation over to this new data type.

// Our implementation is looking prettuy good now. Implement an or combinator
// to compliment and.