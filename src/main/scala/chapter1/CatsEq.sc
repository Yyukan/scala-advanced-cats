/**
  * Implement an instance of Eq for our running Cat example
  */
import cats.Eq
import cats.instances.all._
import cats.syntax.eq._

case class Cat(name: String, age: Int, color: String)

implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
  cat1.name === cat2.name &&
  cat1.age === cat2.age &&
  cat1.color === cat2.color
}

val cat1 = Cat("Garfield",   38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")
val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

cat1 === cat2
cat1 =!= cat2

optionCat1 === optionCat2
optionCat1 =!= optionCat2