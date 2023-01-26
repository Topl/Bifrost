package co.topl.transactiongenerator

import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.{models => legacyModels}
import legacyModels._
import legacyModels.utility.HasLength.instances.bytesLength
import legacyModels.utility.Sized
import co.topl.proto.{models => protoModels}
import co.topl.proto.models.PropositionContextualHeightLock
import co.topl.transactiongenerator.models.Wallet
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString

package object interpreters {

  val HeightLockOneProposition: Proposition = Propositions.Contextual.HeightLock(1)

  val HeightLockOnePropositionProto: PropositionContextualHeightLock =
    protoModels.PropositionContextualHeightLock(1)

  val HeightLockOneSpendingAddress: SpendingAddress = HeightLockOneProposition.spendingAddress

  val HeightLockOneSpendingAddressProto: protoModels.SpendingAddress = {
    // See SpendingAddressable TODO on SpendingAddressable trait
    val typedEvidence = HeightLockOnePropositionProto.spendingAddress.typedEvidence
    protoModels.SpendingAddress.of(
      Some(
        protoModels
          .TypedEvidence(typedEvidence.typePrefix.toInt, ByteString.copyFrom(typedEvidence.evidence.data.toArray))
      )
    )
  }

  val emptyWallet: Wallet =
    Wallet(
      Map.empty,
      Map(
        HeightLockOneSpendingAddress.typedEvidence ->
        HeightLockOneProposition
      )
    )

  /**
   * Incorporate a Transaction into a Wallet by removing spent outputs and including new outputs.
   */
  def applyTransaction(wallet: Wallet)(transaction: protoModels.Transaction): Wallet = {
    // TODO Wallet spentBoxIds model should change to new protobuf specs and not use boxIdIsomorphism
    val spentBoxIds = transaction.inputs
      .flatMap(_.boxId)
      .map(boxId => co.topl.grpc.boxIdIsomorphism[cats.Id].baMorphism.aToB(boxId))
      .map(_.toEitherT[cats.Id])
      .map(_.bimap(_ => Option.empty[Box.Id], _.some))
      .map(_.value)
      .flatMap(_.toOption)
      .flatten

    val transactionId = transaction.id.asTypedBytes
    val newBoxes = transaction.outputs.zipWithIndex.collect {
      case (output, index)
          if wallet.propositions.contains(
            output.address
              .flatMap(fullAddress =>
                // TODO Wallet spendingAddress model should change to new protobuf specs and not use boxIdIsomorphism
                co.topl.grpc
                  .spendingAddressIsorphism[Option]
                  .baMorphism
                  .aToB(fullAddress.spendingAddress)
                  .flatMap(_.toOption)
                  .map(_.typedEvidence)
              )
              .getOrElse(TypedEvidence.empty)
          ) =>
        val boxId = Box.Id(transactionId, index.toShort)
        val box =
          Box(
            // TODO Wallet spendingAddress model should change to new protobuf specs and not use boxIdIsomorphism
            co.topl.grpc
              .spendingAddressIsorphism[Option]
              .baMorphism
              .aToB(output.address.flatMap(_.spendingAddress))
              .flatMap(_.toOption)
              .map(_.typedEvidence)
              .getOrElse(TypedEvidence.empty),
            co.topl.grpc.boxValueIsomorphism[cats.Id].baMorphism.aToB(output.value).getOrElse(Box.Values.Empty)
          )
        (boxId, box)
    }
    wallet.copy(spendableBoxes = wallet.spendableBoxes -- spentBoxIds ++ newBoxes)
  }

  def simpleFullAddress(spendingAddress: SpendingAddress): FullAddress =
    FullAddress(
      NetworkPrefix(0),
      spendingAddress,
      StakingAddresses.NonStaking,
      Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes.fill(64)(0: Byte)))
    )

  def simpleFullAddressProto(spendingAddress: protoModels.SpendingAddress): protoModels.FullAddress =
    protoModels.FullAddress(
      protoModels.NetworkPrefix(0).some,
      spendingAddress.some,
      protoModels.StakingAddressNonStaking.of(),
      protoModels.ProofKnowledgeEd25519.of(ByteString.copyFrom(Bytes.fill(64)(0: Byte).toArray)).some
    )
}
