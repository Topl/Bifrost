package co.topl.codecs.bytes.tetra

import cats.data.NonEmptySet
import cats.implicits._
import co.topl.codecs.bytes.scodecs._
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.consensus.models.BlockId
import co.topl.crypto.models.SecretKeyKesProduct
import com.google.protobuf.ByteString
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import scodec.Codec

import scala.collection.immutable.SortedSet
import scala.util.Try

trait TetraPersistableCodecs {
  import TetraScodecCodecs._

  implicit def persistableProtobufMessage[T <: GeneratedMessage: GeneratedMessageCompanion]: Persistable[T] =
    new Persistable[T] {
      def persistedBytes(value: T): ByteString = value.toByteString

      def fromPersistedBytes(bytes: ByteString): Either[String, T] =
        Try(implicitly[GeneratedMessageCompanion[T]].parseFrom(bytes.newCodedInput())).toEither
          .leftMap(_.getMessage)
    }

  implicit def persistableSeq[T: Codec]: Persistable[Seq[T]] =
    Persistable.instanceFromCodec(seqCodec[T])

  implicit val persistableTransactionOutputIndices: Persistable[NonEmptySet[Short]] =
    Persistable.instanceFromCodec(
      seqCodec[Short].xmap(s => NonEmptySet.fromSetUnsafe(SortedSet.from(s)), _.toList)
    )

  implicit val persistableLong: Persistable[Long] =
    Persistable.instanceFromCodec(longCodec)

  implicit val persistableBigInt: Persistable[BigInt] =
    Persistable.instanceFromCodec

  implicit val persistableUnit: Persistable[Unit] =
    new Persistable[Unit] {
      def persistedBytes(value: Unit): ByteString = ByteString.copyFrom(Array[Byte](0))

      def fromPersistedBytes(bytes: ByteString): Either[String, Unit] =
        Either.cond(bytes.size() == 1 && bytes.byteAt(0) == (0: Byte), (), "Invalid Unit")
    }

  implicit val persistableHeightIdTuple: Persistable[(Long, BlockId)] =
    Persistable.instanceFromCodec(tupleCodec(longCodec, blockIdCodec))

  implicit val persistableByte: Persistable[Byte] =
    Persistable.instanceFromCodec

  implicit val persistableByteString: Persistable[ByteString] =
    Persistable.instanceFromCodec

  implicit val persistableKesProductSecretKey: Persistable[SecretKeyKesProduct] =
    Persistable.instanceFromCodec
}

object TetraPersistableCodecs extends TetraPersistableCodecs
