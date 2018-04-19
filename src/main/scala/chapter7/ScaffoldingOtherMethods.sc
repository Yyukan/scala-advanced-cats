/**
 foldLeft and foldRight are very general methods. We can use them to implement many of the other high-level sequence
 operations we know. Prove this to yourself by implementing substitutes for List's map, flatMap, filter, and sum methods
 in terms of foldRight
 */

def foldableMap[A, B](list: List[A])(f: A => B): List[B] = {
  list.foldRight(List.empty[B])((x, acc) => f(x) :: acc)
}

def foldableFlatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
  list.foldRight(List.empty[B])((x, acc) => f(x) ::: acc)
}

def foldableFilter[A](list: List[A])(f: A => Boolean): List[A] = {
  list.foldRight(List.empty[A])((x, acc) => if (f(x)) x :: acc else acc)
}

def foldableSum(list: List[Int]): Int = {
  list.foldRight(0)(_ + _)
}

import cats.Monoid
import cats.instances.int._
import cats.instances.string._

def foldableSumMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
  list.foldRight(monoid.empty)(monoid.combine)

foldableMap(List(1, 2, 3, 4, 5))(_ * 2)

foldableFlatMap(List(1, 2, 3, 4, 5))(x => List(x * 2))

foldableFilter(List(1, 2, 3, 4, 5))(_ > 2)

foldableSum(List(1, 2, 3, 4, 5))
foldableSumMonoid(List(1, 2, 3, 4, 5))
foldableSumMonoid(List("a", "b", "c", "d", "e"))
