package co.topl.codecs.bytes.tetra

import cats.data.{NonEmptyChain, NonEmptySet}
import co.topl.codecs.bytes.scodecs._
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.models._
import scodec.bits.ByteVector

trait TetraPersistableCodecs {
  import TetraScodecCodecs._

  implicit val persistableCurve25519SecretKey: Persistable[SecretKeys.Curve25519] = Persistable.instanceFromCodec
  implicit val persistableEd25519SecretKey: Persistable[SecretKeys.Ed25519] = Persistable.instanceFromCodec

  implicit val persistableExtendedEd25519SecretKey: Persistable[SecretKeys.ExtendedEd25519] =
    Persistable.instanceFromCodec

  implicit val persistableKesProductSecretKey: Persistable[SecretKeys.KesProduct] =
    Persistable.instanceFromCodec

  implicit val persistableTypedIdentifier: Persistable[TypedIdentifier] =
    Persistable.instanceFromCodec

  implicit val persistableSlotData: Persistable[SlotData] =
    Persistable.instanceFromCodec

  implicit val persistableBlockHeader: Persistable[BlockHeader] =
    Persistable.instanceFromCodec

  implicit val persistableBlockBody: Persistable[BlockBody] =
    Persistable.instanceFromCodec

  implicit val persistableTransaction: Persistable[Transaction] =
    Persistable.instanceFromCodec

  implicit val persistableTransactionOutputIndices: Persistable[NonEmptySet[Short]] =
    Persistable.instanceFromCodec(
      nonEmptyChainCodec[Short].xmap(_.toNes, s => NonEmptyChain.fromNonEmptyList(s.toNonEmptyList))
    )

  implicit val persistableLong: Persistable[Long] =
    Persistable.instanceFromCodec(longCodec)

  implicit val persistableStakingAddressesOperator: Persistable[StakingAddresses.Operator] =
    Persistable.instanceFromCodec

  implicit val persistableInt128: Persistable[Int128] =
    Persistable.instanceFromCodec

  implicit val persistableUnit: Persistable[Unit] =
    new Persistable[Unit] {
      def persistedBytes(value: Unit): Bytes = ByteVector.fromByte(0)

      def fromPersistedBytes(bytes: Bytes): Either[String, Unit] =
        Either.cond(bytes.length == 1 && bytes.head == (0: Byte), (), "Invalid Unit")
    }

  implicit val persistableHeightIdTuple: Persistable[(Long, TypedIdentifier)] =
    Persistable.instanceFromCodec((longCodec :: typedBytesCodec).as[(Long, TypedIdentifier)])

  implicit val persistableBoxValueOperatorRegistration: Persistable[Box.Values.Registrations.Operator] =
    Persistable.instanceFromCodec

  implicit val persistableByte: Persistable[Byte] =
    Persistable.instanceFromCodec
}

object TetraPersistableCodecs extends TetraPersistableCodecs
