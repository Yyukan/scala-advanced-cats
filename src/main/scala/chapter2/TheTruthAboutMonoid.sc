
/**
  * Consider Boolean. How many monoids can you define for this type?
  * For each monoid, define the combine and empty operations and
  * convince yourself that the monoid laws hold.
  * Use the following definitions as a starting point:
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

  implicit val booleanAnd = new Monoid[Boolean] {
    override def empty = true

    override def combine(x: Boolean, y: Boolean) = x && y
  }

  implicit val booleanOr = new Monoid[Boolean] {
    override def empty = false

    override def combine(x: Boolean, y: Boolean) = x || y
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

associativeLaw(true, false, true)(Monoid.booleanAnd)
identityLaw(true)(Monoid.booleanAnd)
identityLaw(false)(Monoid.booleanAnd)

associativeLaw(true, false, true)(Monoid.booleanOr)
identityLaw(true)(Monoid.booleanOr)
identityLaw(false)(Monoid.booleanOr)


