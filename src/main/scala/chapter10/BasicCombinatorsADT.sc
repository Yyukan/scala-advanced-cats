/**
 Check is a function from [A] to Either error or [A]. Function and() should accumulate errors
 using Semigroup.
 Another implementation using ADT
*/
import cats.Semigroup
import cats.syntax.semigroup._
import cats.instances.all._

trait Validation[E, A] {

  def apply(value: A)(implicit s: Semigroup[E]): Either[E, A] = {

    def combine(a: Either[E, A], b: Either[E, A]): Either[E, A] = (a, b) match {
      case (Right(_), Right(_)) => Right(value)
      case (Left(e), Right(_))  => Left(e)
      case (Right(_), Left(e))  => Left(e)
      case (Left(e1), Left(e2)) => Left(e1 |+| e2)
    }

    this match {
      case Check(f)         => f(value)
      case And(self, other) => combine(self(value), other(value))
    }

  }

  def and(other: Validation[E, A]): Validation[E, A] = And(this, other)

}

case class And[E, A](self: Validation[E, A], other: Validation[E, A]) extends Validation[E, A]

case class Check[E, A](f: A => Either[E, A]) extends Validation[E, A]


val validation: Validation[List[String], Int] = Check {
  value => if (value > 0) Right(value) else Left(List("Value is negative"))
}

val and: Validation[List[String], Int] = validation and Check {
  value => if (value > 100) Right(value) else Left(List("Value should be lower then 100"))
}

validation(5)
validation(-5)

and(-10)
and(200)


