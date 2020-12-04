package co.topl.utils

/** Helper type class to define named methods in abstract classes */
trait HasName[A] {
  def name: String
}

object HasName {
  def apply[A](implicit ev: HasName[A]): HasName[A] = ev
  def instance[A](f: () => String): HasName[A] = new HasName[A] {
    override def name: String = f()
  }
}