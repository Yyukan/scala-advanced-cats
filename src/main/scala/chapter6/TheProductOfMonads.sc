/**
 Implement product in terms of flatMap:
 */
import cats.Monad
import cats.implicits._

def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
  for {
    a <- x
    b <- y
  } yield (a, b)
}

product(Option(1), Option(2))
product(List(1, 2), List(3, 4))

