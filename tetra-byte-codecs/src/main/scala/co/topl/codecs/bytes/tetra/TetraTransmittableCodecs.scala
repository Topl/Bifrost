package co.topl.codecs.bytes.tetra

import cats.implicits._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.consensus.models.BlockId
import co.topl.models.utility.Ratio
import com.google.protobuf.ByteString
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

import scala.util.Try

trait TetraTransmittableCodecs {

  import TetraScodecCodecs._
  import co.topl.codecs.bytes.scodecs._

  implicit def transmittableProtobufMessage[T <: GeneratedMessage: GeneratedMessageCompanion]: Transmittable[T] =
    new Transmittable[T] {
      def transmittableBytes(value: T): ByteString = value.toByteString

      def fromTransmittableBytes(bytes: ByteString): Either[String, T] =
        Try(implicitly[GeneratedMessageCompanion[T]].parseFrom(bytes.toByteArray)).toEither
          .leftMap(_.getMessage)
    }

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec

  implicit val unitTransmittable: Transmittable[Unit] = new Transmittable[Unit] {
    override def transmittableBytes(value:     Unit): ByteString = ByteString.EMPTY
    override def fromTransmittableBytes(bytes: ByteString): Either[String, Unit] = Right(())
  }

  implicit val booleanTransmittable: Transmittable[Boolean] = Transmittable.instanceFromCodec(boolCodec)

  implicit val intTransmittable: Transmittable[Int] = Transmittable.instanceFromCodec(intCodec)

  implicit val longTransmittable: Transmittable[Long] = Transmittable.instanceFromCodec(longCodec)

  implicit val longBlockIdOptTransmittable: Transmittable[(Long, Option[BlockId])] =
    Transmittable.instanceFromCodec(
      (longCodec :: optionCodec[BlockId])
        .as[(Long, Option[BlockId])]
    )

  implicit def optionalTransmittable[T: Transmittable]: Transmittable[Option[T]] =
    new Transmittable[Option[T]] {

      def transmittableBytes(value: Option[T]): ByteString =
        value
          .map(Transmittable[T].transmittableBytes)
          .fold(ZeroBS)(OneBS.concat)

      def fromTransmittableBytes(bytes: ByteString): Either[String, Option[T]] =
        Either
          .cond(bytes.size() > 0, bytes, "Empty Bytes")
          .flatMap(bytes =>
            bytes.byteAt(0) match {
              case 0 => none[T].asRight[String]
              case 1 => Transmittable[T].fromTransmittableBytes(bytes.substring(1)).map(_.some)
              case _ => "Invalid Optional".asLeft[Option[T]]
            }
          )
    }

  val ZeroBS: ByteString = ByteString.copyFrom(Array[Byte](0))
  val OneBS: ByteString = ByteString.copyFrom(Array[Byte](1))
}

object TetraTransmittableCodecs extends TetraTransmittableCodecs
