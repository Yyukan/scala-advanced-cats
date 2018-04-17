
/**
 The Autobots, well-known robots in disguise, frequently send messages during battle requesting the power
 levels of their team mates. This helps them coordinate strategies and launch devastating attacks.
 The message sending method looks like this
 */
import scala.concurrent.Future

// Responses are therefore represented as a stack of monads:
type Response[A] = Future[Either[String, A]]

def getPowerLevel(autobot: String): Response[Int] = ???

// Op mus Prime is getting tired of the nested for comprehensions in his neural matrix.
// Help him by rewriting Response using a monad transformer.

???
// Now test the code by implementing getPowerLevel to retrieve data from a set of imaginary allies.
// Here’s the data we’ll use:
val powerLevels = Map(
  "Jazz"      -> 6,
  "Bumblebee" -> 8,
  "Hot Rod"   -> 10
)
// If an Autobot isn’t in the powerLevels map, return an error message reporting that they were unreachable.
// Include the name in the message for good effect.
???

