/**
  The cutting edge SuperAdder v3.5a-32 is the world’s first choice for adding together numbers.
  The main func on in the program has signature def add(items: List[Int]): Int.
  In a tragic accident this code is deleted! Rewrite the method and save the day!
 */

import cats.Monoid
import cats.instances.int._
import cats.instances.double._
import cats.instances.option._
import cats.syntax.semigroup._

def add(items: List[Int]): Int = items match {
  case Nil => Monoid[Int].empty
  case x :: xs => x |+| add(xs)
}

add(List(1, 2, 3, 4))

/**
 SuperAdder’s market share continues to grow, and now there is demand for additional
 functionality. People now want to add {{{ List[Option[Int]] }}}
 Change add so this is possible. The SuperAdder code base is of the highest quality,
 so make sure there is no code duplication!
 */

def addForAnyType[A](items: List[A])(implicit monoid: Monoid[A]): A = items match {
  case Nil => monoid.empty
  case x :: xs => x |+| addForAnyType(xs)
}

addForAnyType(List(Some(1), Some(2), None, Some(4)))

/**
  SuperAdder is entering the POS (point-of-sale, not the other POS) market.
  Now we want to add up Orders:
  */
case class Order(totalCost: Double, quantity: Double)

implicit val orderMonoid = new Monoid[Order] {
  override def empty = Order(0, 0)
  override def combine(x: Order, y: Order): Order = {
    Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
  }
}

addForAnyType(List(Order(1d, 2d), Order(3d, 4d)))(orderMonoid)
