package co.topl.networking

/**
 * Indicates a type that is used for the purposes of networking.
 *
 * Generally, any Protocol Message or Protocol State type should have a corresponding NetworkTypeTag
 * @tparam T the type used in networking
 */
trait NetworkTypeTag[T] {
  type ValueType = T
  def name: String
}

object NetworkTypeTag {

  def create[T](typeName: String): NetworkTypeTag[T] = new NetworkTypeTag[T] {
    val name: String = typeName
  }
}
