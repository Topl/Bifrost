package co.topl.blockchain.interpreters

import cats.Monad
import cats.data.OptionT
import cats.effect.Resource
import co.topl.algebras.Store
import co.topl.blockchain.algebras.NodeMetadataAlgebra
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import co.topl.models.Bytes
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString

import java.nio.charset.StandardCharsets

object NodeMetadata {

  def make[F[_]: Monad](metadataStore: Store[F, Array[Byte], Array[Byte]]): Resource[F, NodeMetadataAlgebra[F]] =
    Resource.pure(
      new NodeMetadataAlgebra[F] {

        def readAppVersion: F[Option[String]] =
          OptionT(metadataStore.get(Keys.AppVersion))
            .map(new String(_, StandardCharsets.UTF_8))
            .value

        def setAppVersion(version: String): F[Unit] =
          metadataStore.put(Keys.AppVersion, version.getBytes(StandardCharsets.UTF_8))

        def readInitTime: F[Option[Long]] =
          OptionT(metadataStore.get(Keys.InitTime))
            .map(Longs.fromByteArray)
            .value

        def setInitTime(timestamp: Long): F[Unit] =
          metadataStore.put(Keys.InitTime, Longs.toByteArray(timestamp))

        def readStakingRegistrationTransactionId: F[Option[TransactionId]] =
          OptionT(metadataStore.get(Keys.StakingRegistrationTransactionId))
            .map(ByteString.copyFrom)
            .map(TransactionId(_))
            .value

        def setStakingRegistrationTransactionId(id: TransactionId): F[Unit] =
          metadataStore.put(Keys.StakingRegistrationTransactionId, id.value.toByteArray)

        def readStakingRegistrationBlockId: F[Option[BlockId]] =
          OptionT(metadataStore.get(Keys.StakingRegistrationBlockId))
            .map(ByteString.copyFrom)
            .map(BlockId(_))
            .value

        def setStakingRegistrationBlockId(id: BlockId): F[Unit] =
          metadataStore.put(Keys.StakingRegistrationBlockId, id.value.toByteArray)

        override def readP2PSK: F[Option[Bytes]] =
          OptionT(metadataStore.get(Keys.P2PSK))
            .map(ByteString.copyFrom)
            .value

        override def setP2PSK(id: Bytes): F[Unit] =
          metadataStore.put(Keys.P2PSK, id.toByteArray)

      }
    )

  object Keys {
    final val AppVersion = Array[Byte](0)
    final val InitTime = Array[Byte](1)
    final val StakingRegistrationTransactionId = Array[Byte](2)
    final val StakingRegistrationBlockId = Array[Byte](3)
    final val P2PSK = Array[Byte](4)
  }
}
