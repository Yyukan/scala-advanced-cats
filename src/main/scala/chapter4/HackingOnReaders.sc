/**
The classic use of Readers is to build programs that accept a configuration as a parameter.
Let’s ground this with a complete example of a simple login system.
Our configuration will consist of two databases: a list of valid users and a list of their passwords:
*/

case class Db(usernames: Map[Int, String], passwords: Map[String, String])

// Start by creating a type alias DbReader for a Reader that consumes a Db as input. This will make the rest of our code shorter.
import cats.data.Reader
import cats.syntax.applicative._

type DbReader[A] = Reader[Db, A]

// Now create methods that generate DbReaders to look up the username for an Int user ID,
// and look up the password for a String username. The type signatures should be as follows:
def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(db => db.usernames.get(userId))

def checkPassword(username: String, password: String): DbReader[Boolean] =
  Reader(db => db.passwords.get(username).contains(password))

// Finally create a checkLogin method to check the password for a given user ID. The type signature should be as follows:
def checkLogin(userId: Int, password: String): DbReader[Boolean] =
  findUsername(userId).flatMap {
    case Some(username) => checkPassword(username, password)
    case None => false.pure[DbReader]
  }

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)
val passwords = Map(
  "dade"  -> "zerocool",
  "kate"  -> "acidburn",
  "margo" -> "secret")

val db = Db(users, passwords)
checkLogin(1, "zerocool").run(db)
checkLogin(4, "davinci").run(db)
