package co.topl.codecs.bytes.tetra

import cats.data.Chain
import co.topl.codecs.bytes.tetra.TetraImmutableCodecs._
import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.{models => legacyModels}
import legacyModels._
import co.topl.consensus.models.BlockHeader
import co.topl.models.utility.ReplaceModelUtil
import co.topl.proto.models.Transaction

trait TetraIdentifiableInstances {

  implicit val identifiableConsensusBlockHeader: Identifiable[BlockHeader] =
    (header: BlockHeader) => (IdentifierTypes.Block.HeaderV2, new Blake2b256().hash(header.immutableBytes))

  implicit val identifiableLegacyBlockHeader: Identifiable[legacyModels.BlockHeader] =
    header => identifiableConsensusBlockHeader.idOf(ReplaceModelUtil.consensusHeader(header))

  implicit val transactionProtoIdentifiable: Identifiable[Transaction] =
    transaction => {
      val bytes =
        legacyModels.Transaction
          .UnprovenProto(
            inputs = Chain.fromSeq(
              transaction.inputs.map(i => legacyModels.Transaction.Unproven.InputProto(i.boxId, i.proposition, i.value))
            ),
            outputs = Chain.fromSeq(transaction.outputs),
            transaction.schedule,
            transaction.data
          )
          .immutableBytes
      val hash = new Blake2b256().hash(bytes)
      (IdentifierTypes.Transaction, hash)
    }

  implicit val transactionIdentifiable: Identifiable[legacyModels.Transaction] =
    transaction =>
      transactionProtoIdentifiable.idOf(
        co.topl.models.utility.transactionIsomorphism[cats.Id].abMorphism.aToB(transaction) match {
          case Right(id)   => id
          case Left(error) => throw new IllegalArgumentException(error)
        }
      )
}

object TetraIdentifiableInstances extends TetraIdentifiableInstances
