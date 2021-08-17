package co.topl.models

object Sized {

  class Strict[Data, Length] private[Sized] (val data: Data) extends AnyVal {
    type L = Length
  }

  class Max[Data, Length] private[Sized] (val data: Data) extends AnyVal {
    type L = Length
  }

  def strict[Data: HasLength, L <: Length](data: Data, length: L): Either[InvalidLength, Strict[Data, L]] = {
    val dataLength = implicitly[HasLength[Data]].length(data)
    Either.cond(
      dataLength == length.value,
      new Strict(data),
      InvalidLength(dataLength)
    )
  }

  def max[Data: HasLength, L <: Length](data: Data, length: L): Either[InvalidLength, Max[Data, L]] = {
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
  case object Empty extends Length(0)
  case object `1` extends Length(1)
  case object `2` extends Length(2)
  case object `4` extends Length(4)
  case object `32` extends Length(32)
  case object `58` extends Length(58)
  case object `64` extends Length(64)
  case object `127` extends Length(127)
  case object `128` extends Length(128)
}

trait HasLength[T] {
  def length(t: T): Int
}

object HasLength {

  trait Instances {

    implicit def arrayLength[T]: HasLength[Array[T]] =
      _.length

    implicit val stringLength: HasLength[String] =
      _.length
  }

  object implicits extends Instances

}

object QuickTest extends App {
  import HasLength.implicits._

  val strictStringTest: Either[Sized.InvalidLength, Sized.Strict[String, Lengths.`4`.type]] =
    Sized.strict("Test", Lengths.`4`)

  val maxStringTest: Either[Sized.InvalidLength, Sized.Max[String, Lengths.`4`.type]] =
    Sized.max("a", Lengths.`4`)

  val strictArrayTest: Either[Sized.InvalidLength, Sized.Strict[Array[Byte], Lengths.`32`.type]] =
    Sized.strict(Array[Byte](1, 2, 3, 4), Lengths.`32`) // Should result in a Left
}
