/**
 Let’s get used to Validated by implementing a simple HTML registration form.
 We receive request data from the client in a Map[String, String] and we want to parse it to create a User object:
*/
case class User(name: String, age: Int)

// Our goal is to implement code that parses the incoming data enforcing the following rules:
// the name and age must be specified;
// the name must not be blank;
// the age must be a valid non-negative integer.

import cats.data.Validated

type Form = Map[String, String]
type EitherErrors[A] = Either[List[String], A]
type ValidatedErrors[A] = Validated[List[String], A]

def getValue(form: Form)(field: String): EitherErrors[String] = {
  form.get(field)
      .toRight(List(s"No form field [$field]"))
}

// Next define a method parseInt that consumes a String and parses it as an Int
def parseInt(value: String): EitherErrors[Int] = {
  Right(value)
    .filterOrElse(_.forall(c => c.isDigit), List(s"Value $value is not a string"))
    .map(_.toInt)
}

// Next implement the validation checks: nonBlank to check Strings, and nonNegative to check Ints
def nonBlank(value: String): EitherErrors[String] = {
  Right(value)
    .filterOrElse(_.nonEmpty, List("Value is empty"))
}

def nonNegative(value: Int): EitherErrors[Int] = {
  Right(value)
    .filterOrElse(_ >= 0, List("Value is negative"))
}

getValue(Map("A" -> "a"))("A")
getValue(Map("A" -> "a"))("B")

parseInt("123")
parseInt("abc")

nonBlank("abc")
nonBlank("")

nonNegative(0)
nonNegative(-1)

// Now combine getValue, parseInt, nonBlank and nonNegative to create readName and readAge
def readName(form: Form): EitherErrors[String] = {
  getValue(form)("name")
    .flatMap(nonBlank)
}

def readAge(form: Form): EitherErrors[Int] = {
  getValue(form)("age")
    .flatMap(nonBlank)
    .flatMap(parseInt)
    .flatMap(nonNegative)
}

readName(Map("name" -> "Alex"))
readName(Map("lastname" -> "Alex"))
readName(Map("name" -> ""))

readAge(Map("age" -> "10"))
readAge(Map("age" -> ""))
readAge(Map("age" -> "abc"))
readAge(Map("age" -> "-1"))

// Finally, use a Semigroupal to combine the results of readName and readAge to produce a User.
// Make sure you switch from Either to Validated to accumulate errors.

import cats.syntax.either._
import cats.syntax.apply._
import cats.instances.list._

def createUser(form: Form): ValidatedErrors[User] = {
  val name: ValidatedErrors[String] = readName(form).toValidated
  val age: ValidatedErrors[Int] = readAge(form).toValidated

  (name, age).mapN(User.apply)
}

createUser(Map("name" -> "Alex", "age" -> "37"))
createUser(Map("name" -> "", "age" -> "-37"))































