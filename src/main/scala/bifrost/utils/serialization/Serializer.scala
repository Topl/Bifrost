package bifrost.utils.serialization

trait Serializer[TFamily, T <: TFamily, R <: Reader, W <: Writer] {

  def serialize(obj: T, w: W): Unit

  def parse(r: R): TFamily
}
