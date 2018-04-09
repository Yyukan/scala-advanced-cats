/**
The State monad allows us to implement simple interpreters for complex expressions, passing the values
of mutable registers along with the result. We can see a simple example of this by implementing
a calculator for post-order integer arithmetic expressions.
*/


import cats.data.State
import cats.syntax.applicative._
import javax.naming.OperationNotSupportedException

type CalcState[A] = State[List[Int], A]

// Start by writing a func on evalOne that parses a single symbol into an instance of State.
def evalOne(symbol: String): CalcState[Int] = symbol match {
  case "+" => State[List[Int], Int] {
    case x :: y :: tail => ((x + y) :: tail, x + y)
    case _ => throw new OperationNotSupportedException()
  }
  case "*" => State[List[Int], Int] {
    case x :: y :: tail => ((x * y) :: tail, x * y)
    case _ => throw new OperationNotSupportedException()
  }
  case x if x forall Character.isDigit => State[List[Int], Int] { state =>
    (symbol.toInt :: state, symbol.toInt)
  }
  case _ => throw new OperationNotSupportedException()
}

evalOne("42").runA(Nil).value

var program = for {
  _   <- evalOne("1")
  _   <- evalOne("2")
  ans <- evalOne("+")
} yield ans

program.runA(Nil).value

// Generalise this example by writing an evalAll method that computes the result of a List[String].
// Use evalOne to process each symbol, and thread the resulting State monads together using flatMap.
// Your func on should have the following signature:

def evalAll(input: List[String]): CalcState[Int] = {
   input.foldLeft(0.pure[CalcState])( (x, y) => x.flatMap(_ => evalOne(y)) )
}

program = evalAll(List("1", "2", "+", "3", "*"))

program.runA(Nil).value

program = for {
  _   <- evalAll(List("1", "2", "+"))
  _   <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans

program.runA(Nil).value

//// Complete the exercise by implementing an evalInput func on that splits an input String into symbols,
//// calls evalAll, and runs the result with an initial stack
def evalInput(input: String): Int = {
  evalAll(input.split(" ").toList).runA(Nil).value
}

evalInput("1 2 + 3 4 + *")


