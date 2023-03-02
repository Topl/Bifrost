package co.topl.codecs.bytes.tetra

import cats.implicits._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.utility.Ratio
import co.topl.{models => legacyModels}
import com.google.protobuf.ByteString
import legacyModels._
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
        Try(implicitly[GeneratedMessageCompanion[T]].parseFrom(bytes.newCodedInput())).toEither
          .leftMap(_.getMessage)
    }

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec
  implicit val typedIdentifierTransmittable: Transmittable[TypedBytes] = Transmittable.instanceFromCodec

  implicit val longTypedIdentifierOptTransmittable: Transmittable[(Long, Option[TypedIdentifier])] =
    Transmittable.instanceFromCodec(
      (longCodec :: optionCodec[TypedIdentifier])
        .as[(Long, Option[TypedIdentifier])]
    )
}

object TetraTransmittableCodecs extends TetraTransmittableCodecs
