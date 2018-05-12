/**
 Check is a function from [A] to Either error or [A]. Function and() should accumulate errors using Semigroup.
 Another implementation using ADT with cats Validated instead of Either
 */
import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import cats.syntax.all._
import cats.instances.all._

trait Validation[E, A] {

  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] = {

    this match {
      case Check(f)         => f(value)
      case And(self, other) => (self(value), other(value)).mapN((_, _) => value)
    }

  }

  def and(other: Validation[E, A]): Validation[E, A] = And(this, other)

}

case class And[E, A](self: Validation[E, A], other: Validation[E, A]) extends Validation[E, A]

case class Check[E, A](f: A => Validated[E, A]) extends Validation[E, A]


val validation: Validation[List[String], Int] = Check {
  value => if (value > 0) Valid(value) else Invalid(List("Value is negative"))
}

val and: Validation[List[String], Int] = validation and Check {
  value => if (value > 100) Valid(value) else Invalid(List("Value should be lower then 100"))
}

validation(5)
validation(-5)

and(-10)
and(200)


