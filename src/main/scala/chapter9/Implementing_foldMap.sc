// Start by writing out the signature of foldMap. It should accept the following parameters:
// a sequence of type Vector(A)
// a function of type A => B, where there is a Monoid for B;
// You will have to add implicit parameters or context bounds to complete the type signature.
import cats.Monoid
import cats.syntax.monoid._

// Now implement the body of foldMap
def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B = {
  seq
    .map(f)
    .foldLeft(Monoid[B].empty)((acc, item) => acc.combine(item))
}

import cats.instances.int._ // for Monoid

foldMap(Vector(1, 2, 3))(identity)

import cats.instances.string._ // for Monoid

foldMap(Vector(1, 2, 3))(_.toString + "! ")

foldMap("Hello world!".toVector)(_.toString.toUpperCase)

