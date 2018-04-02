/**
  * What monoids and semigroups are there for sets?
  */
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] =
    monoid

  implicit def monoidOfSet[A] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]) = x ++ y
  }
}

def associativeLaw[A](x: A, y: A, z: A)
                     (implicit m: Monoid[A]): Boolean = {
  m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
}
def identityLaw[A](x: A)
                  (implicit m: Monoid[A]): Boolean = {
  (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
}

associativeLaw(Set(1), Set(2), Set(3))(Monoid.monoidOfSet)
associativeLaw(Set("a"), Set("b"), Set("c"))(Monoid.monoidOfSet)
identityLaw(Set(1))(Monoid.monoidOfSet)
identityLaw(Set("b"))(Monoid.monoidOfSet)

