/**
 Let’s turn our a en on to UptimeService. We need to rewrite it to abstract over the two types of UptimeClient. We’ll do this in two stages: first we’ll rewrite the class and method signatures,
 then the method bodies. Starting with the method signatures:
*/

import cats.Id

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

// comment out the body of getTotalUptime (replace it with ??? to make everything compile);
// add a type parameter F[_] to UptimeService and pass it on to UptimeClient.

import cats.instances.list._  // for Traverse
import cats.syntax.traverse._ // for traverse
import cats.syntax.functor._  // for map
import cats.Applicative

// There is an applicative for Future so that was fine.
// In this version we are traversing a List[F[Int]].
// We need to prove to the compiler that F has an Applicative.
// Do this by adding an implicit constructor parameter to UptimeService.

class UptimeService[F[_]](client: UptimeClient[F])(implicit applicative: Applicative[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] = {
    hostnames.traverse(client.getUptime).map(_.sum)
  }
}

trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int]
}

class TestUptimeClientFlesh(hosts: Map[String, Int]) extends TestUptimeClient
{
  def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}

def testTotalUptime() = {
  val hosts = Map("host1" -> 10, "host2" -> 6)
  val client = new TestUptimeClientFlesh(hosts)
  val service = new UptimeService(client)
  val actual = service.getTotalUptime(hosts.keys.toList)
  val expected = hosts.values.sum
  assert(actual == expected)
}

testTotalUptime()