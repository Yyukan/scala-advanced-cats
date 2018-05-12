/**
 Check is a function from [A] to Either error or [A]. Function and() should accumulate errors
 using Semigroup.
 */
import cats.Semigroup
import cats.syntax.semigroup._
import cats.instances.all._

type Check[E, A] = A => Either[E, A]

case class Validation[E, A](f: Check[E, A]) {

  def apply(value: A): Either[E, A] = f(value)

  def and(other: Validation[E, A])(implicit s: Semigroup[E]): Validation[E, A] = {

    val func: Check[E, A] = { a => (this(a), other(a)) match {
      case (Right(_), Right(_)) => Right(a)
      case (Left(e), Right(_))  => Left(e)
      case (Right(_), Left(e))  => Left(e)
      case (Left(e1), Left(e2)) => Left(e1 |+| e2)
      }
    }

    Validation(func)
  }
}

val validation = Validation[List[String], Int] {
  value => if (value > 0) Right(value) else Left(List("Value is negative"))
}

val and = validation and Validation[List[String], Int] {
  value => if (value > 100) Right(value) else Left(List("Value should be lower then 100"))
}

validation(5)
validation(-5)

and(-10)
and(200)
