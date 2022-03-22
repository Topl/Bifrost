package co.topl.networking

@simulacrum.typeclass
trait NetworkTypeTag[T]

object NetworkTypeTag {
  def create[T]: NetworkTypeTag[T] = new NetworkTypeTag[T] {}
}
