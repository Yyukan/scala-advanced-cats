/**
  * We need to implement two versions of UptimeClient:
  * an asynchronous one for use in production and
  * a synchronous one for use in our unit tests
  */

import cats.Id
import scala.language.higherKinds
import scala.concurrent.Future

// write a trait definition for UptimeClient that accepts a type constructor F[_] as a parameter;
trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[_]
}

// extend it with two traits, RealUptimeClient and TestUptimeClient, that bind F to Future and Id respectively;
trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}
trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int]
}

// You should now be able to flesh your definition of TestUptimeClient out
//into a full class based on a Map[String, Int] as before.

class TestUptimeClientFlesh(hosts: Map[String, Int]) extends TestUptimeClient
{
  def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}

val client = new TestUptimeClientFlesh(Map("127.0.0.1" -> 19999))

client.getUptime("127.0.0.1")
client.getUptime("no host")