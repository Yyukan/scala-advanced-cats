
/**
 The Autobots, well-known robots in disguise, frequently send messages during battle requesting the power
 levels of their team mates. This helps them coordinate strategies and launch devastating attacks.
 The message sending method looks like this
 */
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.EitherT
import cats.implicits._

// Responses are therefore represented as a stack of monads:
// type Response[A] = Future[Either[String, A]]

// Optimus Prime is getting tired of the nested for comprehensions in his neural matrix.
// Help him by rewriting Response using a monad transformer.

type Response[A] = EitherT[Future, String, A]

// Now test the code by implementing getPowerLevel to retrieve data from a set of imaginary allies.
// Here’s the data we’ll use:
val powerLevels = Map(
  "Jazz"      -> 6,
  "Bumblebee" -> 8,
  "Hot Rod"   -> 10
)

def getPowerLevel(autobot: String): Response[Int] = {
  EitherT[Future, String, Int](
    Future {
      powerLevels
        .get(autobot)
        .map(Right(_))
        .getOrElse(Left(s"No power levels for [$autobot]"))
    }
  )
}

// If an Autobot isn’t in the powerLevels map, return an error message reporting that they were unreachable.
// Include the name in the message for good effect.

Await.result(getPowerLevel("Hot Rod").value, 1 second)
Await.result(getPowerLevel("absent").value, 1 second)

// Two autobots can perform a special move if their combined power level is greater than 15.
// Write a second method, canSpecialMove, that accepts the names of two allies and checks whether a special
// move is possible. If either ally is unavailable, fail with an appropriate error message:
def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
  for {
      level1 <- getPowerLevel(ally1)
      level2 <- getPowerLevel(ally2)
  } yield level1 + level2 > 15
}

// Finally, write a method tacticalReport that takes two ally names and prints a message saying whether they can perform a special move:
def tacticalReport(ally1: String, ally2: String): String = {

  Await.result(canSpecialMove(ally1, ally2).value, 1 second) match {
    case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
    case Right(false) => s"$ally1 and $ally2 need a recharge."
    case Left(f)      => s"Comms error: $f"
  }

}

tacticalReport("Jazz", "Bumblebee")
tacticalReport("Bumblebee", "Hot Rod")
tacticalReport("Jazz", "Ironhide")