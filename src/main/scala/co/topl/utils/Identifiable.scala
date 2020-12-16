package co.topl.utils

/** Helper type class to define named methods in abstract classes */
trait Identifiable[A] {
  def typeString: String
  def typePrefix: Byte
}

object Identifiable {
  def apply[A](implicit ev: Identifiable[A]): Identifiable[A] = ev
  def instance[A](f: () => String, y: () => Byte): Identifiable[A] = new Identifiable[A] {
    override def typeString: String = f()
    override def typePrefix: Byte = y()
  }

  object Syntax {
    implicit final class Ops[A: Identifiable](private val value: A) {
      def typeString: String = Identifiable[A].typeString
      def typePrefix: Byte = Identifiable[A].typePrefix
    }
  }
}
