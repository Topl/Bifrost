package co.topl.utils.codecs.binary.legacy

import scala.util.Try

trait Serializer[TFamily, T <: TFamily, R <: Reader, W <: Writer] {

  def serialize(obj: T, w: W): Unit

  def parse(r: R): TFamily

  def parseTry(r: R): Try[TFamily] =
    Try(parse(r))
}
