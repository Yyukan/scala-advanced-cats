/**
  * Implement pure, map, and flatMap for Id!
  * What interesting discoveries do you uncover about the implementation?
  */

import cats.Id

def pure[A](value: A): Id[A] = value

def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)

def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

pure(123)
map(pure(123))(_.toString)
flatMap(pure(456))(_.toDouble)