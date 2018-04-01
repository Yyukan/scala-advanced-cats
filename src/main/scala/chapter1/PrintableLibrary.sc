/**
  Scala provides a toString method to let us convert any value to a String.
  However, this method comes with a few disadvantages:
    it is implemented for every type in the language, many implementation are of limited use,
    and we can’t opt-in to specific implementations for specific types.
  */

// 1. Define a typeclass Printable[A] containing a single methodformat.
// format should accept a value of type A and return a String.
trait Printable[A] {
  def format(value: A): String
}

// 2. Create an object PrintableInstances containing instances of
// Printable for String and Int.
object PrintableInstances {
  implicit val stringOps = new Printable[String] {
    override def format(value: String) = value
  }

  implicit val intOps = new Printable[Int] {
    override def format(value: Int) = value.toString
  }
}

// 3. Define an object Printable with two generic interface methods:
// format accepts a value of type A and a Printable of the corresponding type.
// It uses the relevant Printable to convert the A to a String.
// print accepts the same parameters as format and returns Unit.
// It prints the A value to the console using println.
object Printable {

  def format[A](value: A)(implicit typeClass: Printable[A]): String = {
    typeClass.format(value)
  }

  def print[A](value: A)(implicit typeClass: Printable[A]): Unit = {
    println(typeClass.format(value))
  }

}

import PrintableInstances._

Printable.print("hello")
Printable.print(99)

Printable.print(Printable.format("hello"))
Printable.print(Printable.format(99))

// ===== Using the Library
final case class Cat(name: String, age: Int, color: String)
//
//implicit val catOps = new Printable[Cat] {
//  override def format(c: Cat) = s"${c.name} is a ${c.age} year-old ${c.color} cat."
//}
//
//Printable.print(Cat("Pussy", 2, "white"))
//
//// ===== Better Syntax
//object PrintableSyntax {
//  implicit class PrintOps[A](value: A) {
//    def format(implicit typeClass: Printable[A]): String = {
//      typeClass.format(value)
//    }
//
//    def print(implicit typeClass: Printable[A]): Unit = {
//      println(typeClass.format(value))
//    }
//  }
//}
//
//import PrintableSyntax._
//
//Cat("Kitty", 3, "black").print
//Cat("Nikki", 4, "red").format
//
//// ===== Using Show
//import cats.Show
//import cats.instances.int._
//import cats.instances.string._
//import cats.syntax.show._
//
//implicit val catShowOps = Show.show[Cat] { cat =>
//  s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat."
//}
//
//println(Cat("Snow", 5, "brown").show)
//
