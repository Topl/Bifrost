package co.topl.networking

/**
 * Indicates a type that is used for the purposes of networking.
 *
 * Generally, any Protocol Message or Protocol State type should have a corresponding NetworkTypeTag
 * @tparam T the type used in networking
 */
trait NetworkTypeTag[T]

object NetworkTypeTag {
  def create[T]: NetworkTypeTag[T] = new NetworkTypeTag[T] {}
}
