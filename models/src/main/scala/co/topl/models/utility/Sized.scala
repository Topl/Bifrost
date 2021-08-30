package co.topl.models.utility

import co.topl.models.{Bytes, TypedBytes}
import co.topl.models.utility.StringDataTypes.Latin1Data

object Sized {

  class Strict[Data, Length] private[Sized] (val data: Data) extends AnyVal {
    type L = Length
  }

  class Max[Data, Length] private[Sized] (val data: Data) extends AnyVal {
    type L = Length
  }

  def strict[Data: HasLength, L <: Length](data: Data)(implicit length: L): Either[InvalidLength, Strict[Data, L]] = {
    val dataLength = implicitly[HasLength[Data]].length(data)
    Either.cond(
      dataLength == length.value,
      new Strict(data),
      InvalidLength(dataLength)
    )
  }

  def max[Data: HasLength, L <: Length](data: Data)(implicit length: L): Either[InvalidLength, Max[Data, L]] = {
    val dataLength = implicitly[HasLength[Data]].length(data)
    Either.cond(
      dataLength <= length.value,
      new Max(data),
      InvalidLength(dataLength)
    )
  }

  case class InvalidLength(length: Int)
}

sealed abstract class Length(val value: Int)

object Lengths {
  implicit case object Empty extends Length(0)
  implicit case object `1` extends Length(1)
  implicit case object `2` extends Length(2)
  implicit case object `4` extends Length(4)
  implicit case object `32` extends Length(32)
  implicit case object `33` extends Length(33)
  implicit case object `58` extends Length(58)
  implicit case object `64` extends Length(64)
  implicit case object `80` extends Length(80)
  implicit case object `96` extends Length(96)
  implicit case object `127` extends Length(127)
  implicit case object `128` extends Length(128)
  implicit case object `256` extends Length(256)
  implicit case object `1440` extends Length(1440)
}

trait HasLength[T] {
  def length(t: T): Int
}

object HasLength {

  trait Instances {

    implicit def bytesLength: HasLength[Bytes] =
      _.length

    implicit def arrayLength[T]: HasLength[Array[T]] =
      _.length

    implicit val stringLength: HasLength[String] =
      _.length

    implicit val bigIntLength: HasLength[BigInt] =
      _.bitLength

    implicit val latin1DataLength: HasLength[Latin1Data] =
      _.value.length

    implicit val typedDataLength: HasLength[TypedBytes] =
      _.allBytes.length
  }

  object implicits extends Instances

}
