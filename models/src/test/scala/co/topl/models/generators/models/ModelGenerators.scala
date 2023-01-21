package co.topl.models.generators.models

import co.topl.proto.models._
import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}

/**
 * Model Generators related to: https://github.com/Topl/protobuf-specs/blob/main/models
 */
trait ModelGenerators {

  def byteGen: Gen[Byte] = Gen.choose[Byte](Byte.MinValue, Byte.MaxValue)

  implicit val arbitraryUnknownFieldSet: Arbitrary[scalapb.UnknownFieldSet] =
    Arbitrary(Gen.const(scalapb.UnknownFieldSet.empty))

  implicit val arbitraryByteString: Arbitrary[ByteString] =
    Arbitrary(Arbitrary.arbByte.arbitrary.map(b => Array(b)).map(ByteString.copyFrom))

  val arbitraryPositiveInt128: Arbitrary[Int128] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield Int128(value, scalapb.UnknownFieldSet.empty)
    )

  implicit val arbitraryTransactionId: Arbitrary[TransactionId] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield TransactionId.of(value)
    )

  implicit val arbitraryBoxId: Arbitrary[Box.Id] =
    Arbitrary(
      for {
        transactionId          <- arbitraryTransactionId.arbitrary
        transactionOutputIndex <- Gen.posNum[Short]
      } yield Box.Id.of(Some(transactionId), transactionOutputIndex)
    )

  implicit val arbitraryCurve25519VK: Arbitrary[VerificationKeyCurve25519] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield VerificationKeyCurve25519.of(value)
    )

  implicit val arbitraryPropositionsPermanentlyLocked: Arbitrary[PropositionPermanentlyLocked] =
    Arbitrary(Gen.const(PropositionPermanentlyLocked.of()))

  implicit val arbitraryPropositionKnowledgeCurve25519: Arbitrary[PropositionKnowledgeCurve25519] =
    Arbitrary(
      for {
        vk <- arbitraryCurve25519VK.arbitrary
      } yield PropositionKnowledgeCurve25519.of(Some(vk))
    )

  // TODO implement missing
  implicit val arbitraryProposition: Arbitrary[Proposition] =
    Arbitrary(
      Gen.oneOf(
        implicitly[Arbitrary[PropositionPermanentlyLocked]].arbitrary,
        implicitly[Arbitrary[PropositionKnowledgeCurve25519]].arbitrary
//        implicitly[Arbitrary[PropositionKnowledgeEd25519]].arbitrary,
//        implicitly[Arbitrary[PropositionKnowledgeExtendedEd25519]].arbitrary,
//        implicitly[Arbitrary[PropositionKnowledgeHashLock]].arbitrary,
//        implicitly[Arbitrary[PropositionCompositionalThreshold]].arbitrary,
//        implicitly[Arbitrary[PropositionCompositionalAnd]].arbitrary,
//        implicitly[Arbitrary[PropositionCompositionalOr]].arbitrary,
//        implicitly[Arbitrary[PropositionCompositionalNot]].arbitrary,
//        implicitly[Arbitrary[PropositionContextualHeightLock]].arbitrary,
//        implicitly[Arbitrary[PropositionContextualRequiredTransactionIO]].arbitrary
      )
    )

  implicit val arbitraryProofsFalse: Arbitrary[ProofUndefined] =
    Arbitrary(Gen.const(ProofUndefined.of()))

  implicit val arbitraryProofsKnowledgeCurve25519: Arbitrary[ProofKnowledgeCurve25519] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield ProofKnowledgeCurve25519.of(value)
    )

  // TODO implement missing
  implicit val arbitraryProof: Arbitrary[Proof] =
    Arbitrary(
      Gen.oneOf(
        arbitraryProofsFalse.arbitrary,
        arbitraryProofsKnowledgeCurve25519.arbitrary
//        arbitraryProofsKnowledgeEd25519.arbitrary,
//        arbitraryProofsKnowledgeVrfEd25519.arbitrary,
//        arbitraryProofsKnowledgeHashLock.arbitrary,
//        arbitraryProofsCompositionalThreshold.arbitrary,
//        arbitraryProofsCompositionalAnd.arbitrary,
//        arbitraryProofsCompositionalOr.arbitrary,
//        arbitraryProofsCompositionalNot.arbitrary,
//        arbitraryProofsContextualHeightLock.arbitrary,
//        arbitraryProofsContextualRequiredTransactionIO.arbitrary
      )
    )

  implicit val arbitraryPolyBox: Arbitrary[PolyBoxValue] =
    Arbitrary(arbitraryPositiveInt128.arbitrary.map(int128 => PolyBoxValue.of(Some(int128))))

  implicit val arbitraryBoxValue: Arbitrary[BoxValue] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(EmptyBoxValue.of()),
        arbitraryPolyBox.arbitrary
//        arbitraryArbitBox.arbitrary,
//        arbitraryAssetBox.arbitrary
      )
    )

  def networkPrefixGen: Gen[NetworkPrefix] =
    byteGen.map(NetworkPrefix.of(_))

  def typedEvidenceGen: Gen[TypedEvidence] =
    for {
      prefix   <- byteGen
      evidence <- arbitraryByteString.arbitrary
    } yield TypedEvidence.of(prefix, evidence)

  implicit def arbitrarySpendingAddress: Arbitrary[SpendingAddress] =
    Arbitrary(
      for {
        typedEvidence <- typedEvidenceGen
      } yield SpendingAddress.of(Some(typedEvidence))
    )

  def ed25519VkGen: Gen[VerificationKeyEd25519] =
    for {
      value <- arbitraryByteString.arbitrary
    } yield VerificationKeyEd25519.of(value)

  implicit val proofsKnowledgeEd25519: Gen[ProofKnowledgeEd25519] =
    for {
      value <- arbitraryByteString.arbitrary
    } yield ProofKnowledgeEd25519.of(value)

  def operatorStakingAddressGen: Gen[StakingAddressOperator] =
    for {
      poolVK <- ed25519VkGen
    } yield StakingAddressOperator.of(Some(poolVK))

  def stakingAddressGen: Gen[StakingAddress] =
    operatorStakingAddressGen

  implicit val arbitraryFullAddress: Arbitrary[FullAddress] =
    Arbitrary(
      for {
        prefix   <- networkPrefixGen
        spending <- arbitrarySpendingAddress.arbitrary
        staking  <- stakingAddressGen
        binding  <- proofsKnowledgeEd25519
      } yield FullAddress.of(Some(prefix), Some(spending), staking, Some(binding))
    )

  implicit val arbitraryTransactionInput: Arbitrary[Transaction.Input] =
    Arbitrary(
      for {
        boxId       <- arbitraryBoxId.arbitrary
        proposition <- arbitraryProposition.arbitrary
        proof       <- arbitraryProof.arbitrary
        value       <- arbitraryBoxValue.arbitrary
      } yield Transaction.Input.of(Some(boxId), proposition, proof, value)
    )

  implicit val arbitraryTransactionOutput: Arbitrary[Transaction.UnspentOutput] =
    Arbitrary(
      for {
        address  <- arbitraryFullAddress.arbitrary
        value    <- arbitraryBoxValue.arbitrary
        minting  <- Gen.prob(0.05)
        metadata <- arbitraryByteString.arbitrary
      } yield Transaction.UnspentOutput.of(Some(address), value, minting, metadata)
    )

  implicit val arbitraryTransactionSchedule: Arbitrary[Transaction.Schedule] =
    Arbitrary(
      for {
        creation    <- Gen.chooseNum[Long](0L, 100000L)
        minimumSlot <- Gen.chooseNum[Long](0L, 50000L)
        maximumSlot <- Gen.chooseNum[Long](minimumSlot, 100000L)
      } yield Transaction.Schedule.of(creation, minimumSlot, maximumSlot)
    )

  implicit val arbitraryTransaction: Arbitrary[Transaction] =
    Arbitrary(
      for {
        inputs <-
          Gen
            .chooseNum[Int](1, 4)
            .flatMap(count =>
              Gen
                .listOfN(count, arbitraryTransactionInput.arbitrary)
            )
        outputs <-
          Gen
            .chooseNum[Int](1, 4)
            .flatMap(count =>
              Gen
                .listOfN(count, arbitraryTransactionOutput.arbitrary)
            )
        schedule <- arbitraryTransactionSchedule.arbitrary
        data     <- arbitraryByteString.arbitrary
      } yield Transaction.of(inputs, outputs, Some(schedule), data)
    )

}
object ModelGenerators extends ModelGenerators
