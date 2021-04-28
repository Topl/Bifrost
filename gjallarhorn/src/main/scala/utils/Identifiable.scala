package utils

case class Identifier(typeString: String, typePrefix: Byte)

/** Helper type class to define named methods in abstract classes */
trait Identifiable[A] { self =>
  //def apply(a: A): Identifier = getId
  def getId: Identifier
}

object Identifiable {
  def apply[A](implicit ev: Identifiable[A]): Identifiable[A] = ev

  def instance[A](f: () => Identifier): Identifiable[A] = new Identifiable[A] {
    override def getId: Identifier = f()
  }

  object Syntax {

    implicit final class Ops[A: Identifiable](private val obj: A) {
      def getId: Identifier = Identifiable[A].getId
    }
  }
}
