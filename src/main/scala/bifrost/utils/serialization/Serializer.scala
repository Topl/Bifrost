package bifrost.utils.serialization

trait Serializer[TFamily, T <: TFamily, R <: Reader, W <: Writer] {

  def toBytes(obj: T, w: W): Unit

  def parseBytes(r: R): TFamily
}
