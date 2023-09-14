package co.topl.byzantine.transactions

import cats.effect.Async
import cats.implicits._
import co.topl.blockchain.StakerInitializers
import co.topl.brambl.builders.locks.PropositionTemplate
import co.topl.brambl.builders.locks.PropositionTemplate.PropositionType
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.models.box.{Attestation, Box, Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models._
import co.topl.brambl.syntax.{groupPolicyAsGroupPolicySyntaxOps, seriesPolicyAsSeriesPolicySyntaxOps}
import co.topl.brambl.wallet.WalletApi
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.numerics.implicits._
import co.topl.quivr.api.Prover
import com.google.protobuf.ByteString
import quivr.models._

object TransactionFactory {

  /**
   * Transacion used to test the following validations
   * - Received syntactically invalid transaction
   * - Processed Transaction
   * - Received duplicate transaction
   *
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction with n outputs
   *  - output 1: HeightRangeLockAddress
   *  - output 2: DigestLockAddress
   *  - output 3: DigitalSignature
   *  - output 4: TickRange
   *  - output 5: BadTickRange
   *  - output 6: GoodAndBadTickRange using threshold
   *  - output 7: GoodBadTickRange using And
   *  - output 8: GoodTickRange using Not
   *  - output n TODO: ExactMatch, LessThan, GreaterThan, EqualTo, Threshold, or
   */
  private[byzantine] def createTransaction_fromGenesisIotx[F[_]: Async](
    inputBoxId:   TransactionOutputAddress,
    box:          Box,
    lvlQuantity1: BigInt
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

      value1 = Value.defaultInstance.withLvl(Value.LVL(lvlQuantity1))
      value_1_Lvl = Value.defaultInstance.withLvl(Value.LVL(1L))

      outputs = List(
        UnspentTransactionOutput(Locks.HeightRangeLockAddress, value1),
        UnspentTransactionOutput(Locks.DigestLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.DigitalSignatureLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.TickRangeLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.BadTickRangeLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.GoodAndBadTickRangeLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.GoodBadTickRangeAndLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.GoodTickRangeNotLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.GoodBadTickRangeOrLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.HeightRangeLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.HeightRangeLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.HeightRangeLockAddress, value_1_Lvl),
        UnspentTransactionOutput(Locks.HeightRangeLockAddress, value_1_Lvl)
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * @param wallet             populated wallet with 1 spendable Box, with TOPLs
   * @param lvlQuantityOutput1 TOPL quntity to transfer
   * @param stakingOperator    which provide the lock address and registration to create the output
   * @return
   */
  private[byzantine] def createTransaction_TOPL[F[_]: Async](
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt,
    stakingOperator:    StakerInitializers.Operator
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant
      (inputBoxId, box) = (wallet.spendableBoxes.head._1, wallet.spendableBoxes.head._2)

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

      outputs = List(
        UnspentTransactionOutput(
          stakingOperator.lockAddress,
          Value.defaultInstance.withTopl(
            Value.TOPL.defaultInstance
              .withQuantity(lvlQuantityOutput1)
              .withRegistration(stakingOperator.registration)
          )
        )
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transacion used to test the following validations,
   * Auth validation, Digest, Signature, TickRange
   * requires that the wallet contains 1 Proposition related with the prover
   *
   * @param wallet it requires that the wallet contains 1 specific proposition related to the prove that we want to test
   * @param lvlQuantityOutput1
   * @param prover
   * @return
   */
  private[byzantine] def createTransaction_usingInput_B[F[_]: Async](
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt,
    prover:             PropositionType
  ): F[IoTransaction] =
    for {

      timestamp <- Async[F].realTimeInstant
      (inputBoxId, box) = (wallet.spendableBoxes.head._1, wallet.spendableBoxes.head._2)
      lockAddress = wallet.propositions.head._1

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)
      attestation = Attestation(Attestation.Value.Predicate(predicate))
      inputs = List(SpentTransactionOutput(inputBoxId, attestation, box.value))

      outputs = List(
        UnspentTransactionOutput(lockAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput1)))
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      // used to test a And, Not proposition template with tick range propositions
      tickProver <- Prover.tickProver[F].prove((), unprovenTransaction.signable)

      proof <-
        prover match {
          case PropositionTemplate.types.Digest =>
            Prover.digestProver[F].prove(Locks.preimage, unprovenTransaction.signable)

          case PropositionTemplate.types.Signature =>
            val witness = Witness(
              ByteString.copyFrom(
                (new ExtendedEd25519)
                  .sign(
                    WalletApi.pbKeyPairToCryotoKeyPair(Locks.childKeyPair).signingKey,
                    unprovenTransaction.signable.value.toByteArray
                  )
              )
            )
            Prover.signatureProver[F].prove(witness, unprovenTransaction.signable)

          case PropositionTemplate.types.Tick =>
            Prover.tickProver[F].prove((), unprovenTransaction.signable)

          // only works for And with tick proposition
          case PropositionTemplate.types.And =>
            Prover.andProver[F].prove((tickProver, tickProver), unprovenTransaction.signable)

          // only works for Not with tick proposition
          case PropositionTemplate.types.Not =>
            Prover.notProver[F].prove(tickProver, unprovenTransaction.signable)

          // only works for Or with tick propositions
          case PropositionTemplate.types.Or =>
            Prover.orProver[F].prove((tickProver, tickProver), unprovenTransaction.signable)

          case _ => throw new MatchError("Invalid prover")
        }

      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Verify that this transaction does not spend the same box more than once
   *
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction
   */
  private[byzantine] def createTransaction_DoubleSpend[F[_]: Async](
    inputBoxId: TransactionOutputAddress,
    box:        Box
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)

      // same items in the list
      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        ),
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(List.empty)
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  private def createTransaction_withFailures[F[_]: Async](
    inputBoxId: TransactionOutputAddress,
    box:        Box,
    outputs:    Seq[UnspentTransactionOutput],
    datum:      Datum.IoTransaction
  ): F[IoTransaction] =
    for {

      predicate <- Attestation.Predicate(box.lock.getPredicate, Nil).pure[F]

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withDatum(datum)

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Verify that this transaction does not contain too many outputs.
   * - DataLengthValidation validates approved transaction data length, includes proofs
   *
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction wich is expected to fail
   */
  private[byzantine] def createTransaction_ManyOutputs[F[_]: Async](
    inputBoxId: TransactionOutputAddress,
    box:        Box
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant
      iotx <- createTransaction_withFailures(
        inputBoxId,
        box,
        // too many outputs
        outputs = (1 to Short.MaxValue).map(_ =>
          UnspentTransactionOutput(
            Locks.HeightRangeLockAddress,
            Value.defaultInstance.withLvl(Value.LVL(BigInt(1)))
          )
        ),
        Datum.IoTransaction(
          Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
        )
      )
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Verify that the timestamp of the transaction is positive (greater than 0).
   * - Verify that the schedule of the timestamp contains valid minimum and maximum slot values
   *
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction wich is expected to fail
   */
  private[byzantine] def createTransaction_BadSchedule[F[_]: Async](
    inputBoxId: TransactionOutputAddress,
    box:        Box,
    min:        Long,
    max:        Long,
    timestamp:  Long
  ): F[IoTransaction] =
    for {
      iotx <- createTransaction_withFailures(
        inputBoxId,
        box,
        outputs = List(
          UnspentTransactionOutput(
            Locks.HeightRangeLockAddress,
            Value.defaultInstance.withLvl(Value.LVL(BigInt(1)))
          )
        ),
        Datum.IoTransaction(Event.IoTransaction(Schedule(min, max, timestamp), SmallData.defaultInstance))
      )
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation
   *
   * @param wallet populated wallet with spendableBoxes
   * @prover one of lockedProved, digestProver, signatureProver ... default to heightProver
   * @return a transaction wich is expected to fail
   */
  private[byzantine] def createTransaction_BadAttestation[F[_]: Async](
    inputBoxId: TransactionOutputAddress,
    box:        Box,
    prover:     PropositionType
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

      outputs = List(
        UnspentTransactionOutput(
          Locks.HeightRangeLockAddress,
          Value.defaultInstance.withLvl(Value.LVL(BigInt(1)))
        )
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      proof <-
        prover match {
          case PropositionTemplate.types.Locked => Prover.lockedProver[F].prove((), unprovenTransaction.signable)
          case PropositionTemplate.types.Digest =>
            Prover.digestProver[F].prove(Preimage(), unprovenTransaction.signable)
          case PropositionTemplate.types.Signature =>
            Prover
              .signatureProver[F]
              .prove(Witness(ByteString.copyFrom(Array.fill(64)(0.byteValue))), unprovenTransaction.signable)
          case PropositionTemplate.types.Tick => Prover.tickProver[F].prove((), unprovenTransaction.signable)
          /**
           * TODO there is no proposition template for the following Provers
           * there is no plans in the immediate future to support these proposition types.
           * It is not currently used anywhere. (The credentialler can not yet prove these types of propositions).
           */
//          case "exactMatchProver"             => Prover.exactMatchProver[F].prove((), unprovenTransaction.signable)
//          case "lessThanProver"               => Prover.lessThanProver[F].prove((), unprovenTransaction.signable)
//          case "greaterThanProver"            => Prover.greaterThanProver[F].prove((), unprovenTransaction.signable)
//          case "equalToProver"                => Prover.equalToProver[F].prove((), unprovenTransaction.signable)
          case PropositionTemplate.types.Threshold =>
            Prover.thresholdProver[F].prove(Set.empty, unprovenTransaction.signable)
          case PropositionTemplate.types.Not => Prover.notProver[F].prove(Proof(), unprovenTransaction.signable)
          case PropositionTemplate.types.And =>
            Prover.andProver[F].prove(Proof() -> Proof(), unprovenTransaction.signable)
          case PropositionTemplate.types.Or =>
            Prover.orProver[F].prove(Proof() -> Proof(), unprovenTransaction.signable)
          case _ => Prover.heightProver[F].prove((), unprovenTransaction.signable)
        }

      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transaction used to test the following validations
   * - Processed Transaction with a Group constructor Transaction Token
   *
   * @param inputBoxId TransactionOutputAddress to attached to the SpentTransactionOutput
   * @param box lock and value
   * @return a transaction with n outputs
   *  - output 1: HeightRangeLockAddress, with a Group constructor token
   */
  private[byzantine] def createTransactionGroupTamV2[F[_]: Async](
    inputBoxId: TransactionOutputAddress,
    box:        Box,
    quantity:   BigInt
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

      groupPolicy = Event.GroupPolicy(label = "Crypto Frogs", inputBoxId)
      outputs = List(
        UnspentTransactionOutput(
          Locks.HeightRangeLockAddress,
          Value.defaultInstance.withGroup(
            Value.Group(
              groupId = groupPolicy.computeId,
              fixedSeries = Option.empty[SeriesId],
              quantity = quantity: Int128
            )
          )
        )
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withGroupPolicies(Seq(Datum.GroupPolicy(groupPolicy)))
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transaction used to test the following validations
   * - Mint Transaction with a Series Constructor Transaction Token
   *
   * @param inputBoxId TransactionOutputAddress to attached to the SpentTransactionOutput
   * @param box lock and value
   * @return a transaction with n outputs
   *  - output 1: HeightRangeLockAddress, Series Constructor token
   */
  private[byzantine] def createTransactionSeriesTamV2[F[_]: Async](
    inputBoxId: TransactionOutputAddress,
    box:        Box,
    quantity:   BigInt
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

      seriesPolicy = Event.SeriesPolicy(label = "Crypto Frogs", registrationUtxo = inputBoxId)
      outputs = List(
        UnspentTransactionOutput(
          Locks.HeightRangeLockAddress,
          Value.defaultInstance.withSeries(
            Value.Series(
              seriesId = seriesPolicy.computeId,
              quantity = quantity: Int128
            )
          )
        )
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withSeriesPolicies(Seq(Datum.SeriesPolicy(seriesPolicy)))
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transaction used to test the following validations
   * - Mint Transaction with a Group and a Series Constructor Transaction Token
   *
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction with n outputs
   *  - output 1: HeightRangeLockAddress, Group
   *  - output 2: HeightRangeLockAddress, Series
   */
  private[byzantine] def createTransactionGroupANDSeriesTamV2[F[_]: Async](
    inputBoxIdGroup:  TransactionOutputAddress,
    inputBoxIdSeries: TransactionOutputAddress,
    lock:             Lock,
    valueInputGroup:  Value,
    valueInputSeries: Value,
    groupQuantity:    BigInt,
    seriesQuantity:   BigInt
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant

      predicate = Attestation.Predicate(lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxIdGroup,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = valueInputGroup
        ),
        SpentTransactionOutput(
          address = inputBoxIdSeries,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = valueInputSeries
        )
      )

      groupPolicy = Event.GroupPolicy(label = "Crypto Frogs", registrationUtxo = inputBoxIdGroup)
      seriesPolicy = Event.SeriesPolicy(label = "Crypto Frogs", registrationUtxo = inputBoxIdSeries)

      outputs = List(
        UnspentTransactionOutput(
          Locks.HeightRangeLockAddress,
          Value.defaultInstance.withGroup(
            Value.Group(
              groupId = groupPolicy.computeId,
              fixedSeries = Option.empty[SeriesId],
              quantity = groupQuantity: Int128
            )
          )
        ),
        UnspentTransactionOutput(
          Locks.HeightRangeLockAddress,
          Value.defaultInstance.withSeries(
            Value.Series(
              seriesId = seriesPolicy.computeId,
              quantity = seriesQuantity: Int128
            )
          )
        )
      )

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withGroupPolicies(Seq(Datum.GroupPolicy(groupPolicy)))
          .withSeriesPolicies(Seq(Datum.SeriesPolicy(seriesPolicy)))
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transaction used to test the following validations
   * - Move a minted Transaction with a Group and a Series Constructor Transaction Token
   *
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction with n outputs
   *  - output 1: HeightRangeLockAddress, Group
   *  - output 2: HeightRangeLockAddress, Series
   */
  private[byzantine] def createTransactionMoveGroupANDSeriesTamV2[F[_]: Async](
    inputBoxIdGroup:  TransactionOutputAddress,
    lock:             Lock,
    valueInputGroup:  Value,
    valueOutputGroup: Value
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant

      predicate = Attestation.Predicate(lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxIdGroup,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = valueInputGroup
        )
      )

      outputs = List(UnspentTransactionOutput(Locks.HeightRangeLockAddress, valueOutputGroup))

      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withDatum(
            Datum.IoTransaction(
              Event.IoTransaction(Schedule(0, Long.MaxValue, timestamp.toEpochMilli), SmallData.defaultInstance)
            )
          )

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  private def proveTransaction(
    unprovenTransaction: IoTransaction,
    predicateWithProof:  Attestation.Predicate
  ): IoTransaction =
    unprovenTransaction.copy(
      inputs = unprovenTransaction.inputs.map(
        _.copy(attestation = Attestation(Attestation.Value.Predicate(predicateWithProof)))
      )
    )

}
