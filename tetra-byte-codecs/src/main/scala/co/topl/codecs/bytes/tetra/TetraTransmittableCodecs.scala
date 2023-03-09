package co.topl.codecs.bytes.tetra

import cats.implicits._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.consensus.models.BlockId
import co.topl.models.utility.Ratio
import com.google.protobuf.ByteString
import scalapb.GeneratedMessage
import scalapb.GeneratedMessageCompanion

import scala.util.Try

trait TetraTransmittableCodecs {

  import TetraScodecCodecs._
  import co.topl.codecs.bytes.scodecs._

  implicit def transmittableProtobufMessage[T <: GeneratedMessage: GeneratedMessageCompanion]: Transmittable[T] =
    new Transmittable[T] {
      def transmittableBytes(value: T): ByteString = value.toByteString

      def fromTransmittableBytes(bytes: ByteString): Either[String, T] =
        Try(implicitly[GeneratedMessageCompanion[T]].parseFrom(bytes.toByteArray)).toEither
          .leftMap(v =>
            //
            v.getMessage
          )
    }

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec

  implicit val longBlockIdOptTransmittable: Transmittable[(Long, Option[BlockId])] =
    Transmittable.instanceFromCodec(
      (longCodec :: optionCodec[BlockId])
        .as[(Long, Option[BlockId])]
    )
}

object TetraTransmittableCodecs extends TetraTransmittableCodecs
