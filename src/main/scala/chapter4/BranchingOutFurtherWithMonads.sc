/**
  * Write a Monad for our Tree data type
  */
sealed trait Tree[+A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
def leaf[A](value: A): Tree[A] = Leaf(value)

/**
  * Tree Monad
  */
import cats.Monad

implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
  override def pure[A](value: A): Tree[A] = leaf(value)

  override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => leaf(f(x))
    case Branch(left, right) => branch(map(left)(f), map(right)(f))
  }

  override def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] = tree match {
    case Leaf(x) => f(x)
    case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
  }

  override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
    f(a) match {
      case Leaf(Left(x))  => tailRecM(x)(f)
      case Leaf(Right(x)) => pure(x)
      case Branch(left, right) =>
        Branch(
          flatMap(left) {
            case Left(x)  => tailRecM(x)(f)
            case Right(x) => pure(x)
          },
          flatMap(right) {
            case Left(x)  => tailRecM(x)(f)
            case Right(x) => pure(x)
          }
        )
    }
  }
}

/**
 Verify that the code works on instances of Branch and Leaf,
 and that the Monad provides Functor-like behaviour for free.
*/
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap

val tree: Tree[Int] = Branch(
  Leaf(1),
  Branch(Leaf(2), Leaf(3)))

tree.map(x => x + 1)
tree.flatMap(x => branch(leaf(x + 1), leaf(x + 2)))

/**
 Also verify that having a Monad in scope allows us to use for comprehensions,
 despite the fact that we haven’t directly implemented flatMap or map on Tree.
*/
// leaf is replaced with branch
for {
  root   <- leaf("root")
  result <- branch(leaf(root + " left"), leaf(root + " right"))
} yield result


