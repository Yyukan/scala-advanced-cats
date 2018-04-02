/**
  * Re-implement the Cat application from the previous sec on using Show instead of Printable.
  */
import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

case class Cat(name: String, age: Int, color: String)

implicit val catShowOps = Show.show[Cat] { cat =>
  s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat."
}

println(Cat("Snow", 5, "brown").show)