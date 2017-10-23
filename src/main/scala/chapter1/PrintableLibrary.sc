
// Printable Library
trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringOps = new Printable[String] {
    override def format(value: String) = value
  }

  implicit val intOps = new Printable[Int] {
    override def format(value: Int) = value.toString
  }
}

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

implicit val catOps = new Printable[Cat] {
  override def format(c: Cat) = s"${c.name} is a ${c.age} year-old ${c.color} cat."
}

Printable.print(Cat("Pussy", 2, "white"))

// ===== Better Syntax
object PrintableSyntax {
  implicit class PrintOps[A](value: A) {
    def format(implicit typeClass: Printable[A]): String = {
      typeClass.format(value)
    }

    def print(implicit typeClass: Printable[A]): Unit = {
      println(typeClass.format(value))
    }
  }
}

import PrintableSyntax._

Cat("Kitty", 3, "black").print
Cat("Nikki", 4, "red").format

// ===== Using Show
import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

implicit val catShowOps = Show.show[Cat] { cat =>
  s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat."
}

println(Cat("Snow", 5, "brown").show)

