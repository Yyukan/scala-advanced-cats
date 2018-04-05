/**
  * Every monad is also a functor.
  * We can define map in the same way for every monad using the
  * existing methods, flatMap and pure.
  * Try defining map yourself now.
  */
import scala.language.higherKinds

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(func andThen pure)
}
