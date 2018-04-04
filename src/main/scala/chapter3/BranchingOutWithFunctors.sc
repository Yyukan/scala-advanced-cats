/**
 Write a Functor for the following binary tree data type.
 Verify that the code works as expected on instances of Branch and Leaf
 */
sealed trait Tree[+A]

case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

import cats.Functor
import cats.syntax.functor._

implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
  override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value)         => Leaf(f(value))
  }
}

val tree: Tree[Int] = Branch(
  Leaf(1),
  Branch(Leaf(2), Leaf(3)))

tree.map(x => x + 100)