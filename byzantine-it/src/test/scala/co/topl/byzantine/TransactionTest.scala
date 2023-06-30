package co.topl.byzantine

import cats.Id
import cats.data.{EitherT, OptionT}
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.brambl.builders.BuilderError
import co.topl.brambl.builders.locks.{LockTemplate, PropositionTemplate}
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.models.box.{Attestation, Box, Challenge, Lock, Value}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, Event, Indices, LockAddress, TransactionOutputAddress}
import co.topl.brambl.syntax.{ioTransactionAsTransactionSyntaxOps, lockAsLockSyntaxOps, transactionIdAsIdSyntaxOps}
import co.topl.brambl.wallet.WalletApi
import co.topl.brambl.wallet.WalletApi.pbKeyPairToCryotoKeyPair
import co.topl.byzantine.util._
import co.topl.crypto.generation.Bip32Indexes
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.interpreters.NodeRpcOps._
import co.topl.numerics.implicits._
import co.topl.quivr.api.{Proposer, Prover}
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import com.spotify.docker.client.DockerClient
import fs2.Stream
import munit.{FailSuiteException, Location}
import org.typelevel.log4cats.Logger
import quivr.models.Proposition.{DigitalSignature, TickRange}
import quivr.models.{Digest, KeyPair, Preimage, Proof, Proposition, SmallData, VerificationKey, Witness}
import scala.concurrent.duration.{Duration, DurationInt}

class TransactionTest extends IntegrationSuite {

  type RpcClient = NodeRpc[F, Stream[F, *]]

  override val munitTimeout: Duration = 3.minutes

  // HeightRange BEGIN
  private val HeightRangeProposition = Proposition(
    Proposition.Value.HeightRange(Proposition.HeightRange("header", 1, Long.MaxValue))
  )

  private val HeightRangeLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(HeightRangeProposition)), 1))
  )

  private val HeightRangeAddress =
    HeightRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // HeightRange END

  // Digest BEGIN
  private val preimage = Preimage(ByteString.copyFrom("secret".getBytes), ByteString.copyFrom("salt".getBytes))

  private val digest = Digest(
    ByteString.copyFrom((new Blake2b256).hash(preimage.input.toByteArray ++ preimage.salt.toByteArray))
  )
  private val DigestProposition = Proposition(Proposition.Value.Digest(Proposition.Digest("Blake2b256", digest)))

  private val DigestLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(DigestProposition)), 1))
  )

  private val DigestAddress =
    DigestLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // Digest END

  // Digital Signature BEGIN
  private val indices = Indices(0, 0, 0)

  private val keyPair: KeyPair =
    WalletApi.cryptoToPbKeyPair((new ExtendedEd25519).deriveKeyPairFromSeed(Array.fill(96)(0: Byte)))

  private val childKeyPair: KeyPair =
    WalletApi.cryptoToPbKeyPair(
      (new ExtendedEd25519).deriveKeyPairFromChildPath(
        WalletApi.pbKeyPairToCryotoKeyPair(keyPair).signingKey,
        List(
          Bip32Indexes.HardenedIndex(indices.x),
          Bip32Indexes.SoftIndex(indices.y),
          Bip32Indexes.SoftIndex(indices.z)
        )
      )
    )

  private val digitalSignature = DigitalSignature("ExtendedEd25519", keyPair.vk)
  private val DigitalSignatureProposition = Proposition(Proposition.Value.DigitalSignature(digitalSignature))

  private val DigitalSignatureLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(DigitalSignatureProposition)), 1))
  )

  private val DigitalSignatureAddress =
    DigitalSignatureLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // Digital Signature END

  // TickRange BEGIN
  //  private val tickRange = TickRange(min = 0L, max = 1L) // test ok
  //  private val tickRange = TickRange(min = 0L, max = 0L) // test ok
  // TODO with the following change, it produces all the iotxs but, block packer fails with EvaluationAuthorizationFailed, tickRange and the digitalSignature? why? talk with Sean about it
  private val tickRange = TickRange(min = 0L, max = -1L)
  private val TickRangeProposition = Proposition(Proposition.Value.TickRange(tickRange))

  private val TickRangeLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(TickRangeProposition)), 1))
  )

  private val TickRangeAddress =
    TickRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // TickRange END

  // Same model that co.topl.transactiongenerator.models, creating it again to not depend on that module
  case class Wallet(
    spendableBoxes: Map[TransactionOutputAddress, Box],
    propositions:   Map[LockAddress, Lock]
  )

  /**
   * Submits many transactions to a group of running node and verifies the transactions are confirmed
   * The following step are being evaluated: No success meaning: broadcast the iotx to the node, produce an error.
   * A. InsufficientInputFunds, if we use a iotx which spent a box from genesis block
   * B. Success, produces 'transaction_1', using a txio which spent a box from genesis block, this iotx will produces N outputs, described on step B
   * C. Received duplicate transaction, if we try to do the same sequencially in step B
   * D. Verify that submit a transaction contains at least one input
   * E. Verify that this transaction does not spend the same box more than once, No Success is expected
   * F. Verify that this transaction contain too many outputs, ExcessiveOutputsCount, DataLengthValidation, No Success is expected
   * G. Verify that the timestamp of the transaction is positive (greater than or equal to 0). No Success is expected
   * H. Verify that the schedule of the timestamp contains valid minimum and maximum slot values. No Success is expected
   * I. Verify that the schedule of the timestamp contains valid minimum and maximum slot values. No Success is expected
   * J. Verify again that the schedule of the timestamp contains valid minimum and maximum slot values. No Success is expected
   * K. Verify that each transaction output contains a positive quantity (where applicable). No Success is expected
   * L. Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation, No Success is expected
   * N. Success, create and broadcast a txio using a wallet which contains a Digest proposition, and consumes input 'transaction_1' (step B)
   * M. Success, create and broadcast a txio using a wallet which contains a DigitalSignature proposition, and consumes input 'transaction_1' (step B)
   * O. Success, create and broadcast a txio using a wallet which contains a TickRange proposition, and consumes input 'transaction_1' (step B)
   * P. TODO
   * Q. TODO
   * R. TODO
   * Z. Finally, fetch node container logs, and assert expected results.
   *    When the test ends, it prints: Writing container logs to byzantine-it/target/logs/TransactionTest-node1-ed2a0569.log, chech it out
   */
  test("Submits many transaction to one node and verifies the transaction is confirmed") {
    val resource =
      for {
        (dockerSupport, _dockerClient) <- DockerSupport.make[F]()
        implicit0(dockerClient: DockerClient) = _dockerClient
        node1 <- dockerSupport.createNode(
          "TransactionTest-node1",
          "TransactionTest",
          TestNodeConfig(genusEnabled = true)
        )
        _ <- node1.startContainer[F].toResource

        node1Client  <- node1.rpcClient[F](node1.config.rpcPort, tls = false)
        genus1Client <- node1.rpcGenusClient[F](node1.config.rpcPort, tls = false)
        _            <- node1Client.waitForRpcStartUp.toResource
        _            <- genus1Client.waitForRpcStartUp.toResource

        _             <- Logger[F].info("Fetching genesis block Genus Grpc Client").toResource
        blockResponse <- genus1Client.blockIdAtHeight(1).toResource

        // a wallet populated with the genesis iotx, safe head
        wallet = createWallet(
          Map(HeightRangeAddress -> HeightRangeLock),
          blockResponse.block.fullBody.transactions.head
        )

        // A. No Success is expected, InsufficientInputFunds create tx from genesis block using: (10000000 lvl in total)
        iotx_A <- createTransaction_1(wallet, 10000000, 1, 1, 1).toResource
        _      <- node1Client.broadcastTransaction(iotx_A).voidError.toResource

        /**
         * B. Success create tx from genesis block using HeightLock: (10000000 -> (9999900, 100))
         *
         * Output1: HeightRangeAddress, 9999900 lvl
         * Output2: DigestAddress, 1 lvl
         * Output3: DigitalSignature, 1 lvl
         * Output4: TickRange, 1 lvl
         */
        iotx_B <- createTransaction_1(wallet, 9999900, 1, 1, 1).toResource
        _      <- node1Client.broadcastTransaction(iotx_B).toResource

        iotxRes_1 <- OptionT(node1Client.fetchTransaction(iotx_B.id))
          .getOrRaise(new FailSuiteException("ERROR! fetchTransaction 1", Location.empty))
          .toResource
        _ <- assertIO((iotxRes_1.outputs(0).value.getLvl.quantity: BigInt).pure[F], BigInt(9999900)).toResource
        _ <- assertIO((iotxRes_1.outputs(1).value.getLvl.quantity: BigInt).pure[F], BigInt(1)).toResource
        _ <- assertIO((iotxRes_1.outputs(2).value.getLvl.quantity: BigInt).pure[F], BigInt(1)).toResource
        _ <- assertIO((iotxRes_1.outputs(3).value.getLvl.quantity: BigInt).pure[F], BigInt(1)).toResource

        // C. No Success, Create the same tx from genesis block using , Received duplicate transaction is expected
        _ <- createTransaction_1(wallet, 9999900, 1, 1, 1).toResource
        _ <- node1Client.broadcastTransaction(iotx_B).toResource

        // D. No Success, Verify that this transaction contains at least one input
        iotx_D <- IoTransaction.defaultInstance.pure[F].toResource
        _      <- node1Client.broadcastTransaction(iotx_D).voidError.toResource

        // E. No Success, Verify that this transaction does not spend the same box more than once
        iotx_E <- createTransaction_2(wallet).toResource
        _      <- node1Client.broadcastTransaction(iotx_E).voidError.toResource

        // F. No Success, Verify that this transaction contain too many outputs, ExcessiveOutputsCount, DataLengthValidation
        iotx_F <- createTransaction_3(wallet).toResource
        _      <- node1Client.broadcastTransaction(iotx_F).voidError.toResource

        // G. No Success, Verify that the timestamp of the transaction is positive (greater than or equal to 0). Bad timestamp
        iotx_G <- createTransaction_4(wallet, min = 0L, max = 0L, timestamp = -1L).toResource
        _      <- node1Client.broadcastTransaction(iotx_G).voidError.toResource

        // H. No Success, Verify that the schedule of the timestamp contains valid minimum and maximum slot values. Bad min
        iotx_H <- createTransaction_4(wallet, min = 1L, max = 0L, timestamp = 0L).toResource
        _      <- node1Client.broadcastTransaction(iotx_H).voidError.toResource

        // I. No Success, Verify that the schedule of the timestamp contains valid minimum and maximum slot values Bad min,max
        iotx_I <- createTransaction_4(wallet, min = -1L, max = 0L, timestamp = 0L).toResource
        _      <- node1Client.broadcastTransaction(iotx_I).voidError.toResource

        // J. No Success, Verify that the schedule of the timestamp contains valid minimum and maximum slot values Bad min,max
        iotx_J <- createTransaction_4(wallet, min = 1L, max = 0L, timestamp = 0L).toResource
        _      <- node1Client.broadcastTransaction(iotx_J).voidError.toResource

        // K. No Success, Verify that each transaction output contains a positive quantity (where applicable)
        iotx_K <- createTransaction_1(wallet, -1L, 1L, 1L, 4).toResource
        _      <- node1Client.broadcastTransaction(iotx_K).voidError.toResource

        // L No Success, Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation
        // No success reason, the wallet used only contains a HeightRange proposition
        iotxs <- Async[F]
          .parSequenceN(1)(
            Seq(
              "lockedProver"      -> 1,
              "digestProver"      -> 2,
              "signatureProver"   -> 3,
              "tickProver"        -> 4,
              "exactMatchProver"  -> 5,
              "lessThanProver"    -> 6,
              "greaterThanProver" -> 7,
              "equalToProver"     -> 8,
              "thresholdProver"   -> 9,
              "notProver"         -> 10,
              "andProver"         -> 11,
              "orProver"          -> 12
            )
              .map { case (prover, i) => createTransaction_5(wallet, prover, i) }
          )
          .toResource

        _ <- Async[F]
          .parSequenceN(1)(iotxs.map(iotx => node1Client.broadcastTransaction(iotx).voidError))
          .void
          .toResource

        // N. Success a wallet which contains a Digest proposition, consumes input  transaction_1
        wallet2 = createWallet(Map(DigestAddress -> DigestLock), iotx_B)
        iotx_N <- createTransaction_1_1(wallet2, 1L).toResource
        _      <- node1Client.broadcastTransaction(iotx_N).toResource

        // M. Success a wallet which contains a DigitalSignature proposition, consumes input  transaction_1
        wallet3 = createWallet(Map(DigitalSignatureAddress -> DigitalSignatureLock), iotx_B)
        iotx_M <- createTransaction_1_2(wallet3, 1L).toResource
        _      <- node1Client.broadcastTransaction(iotx_M).toResource

        // O. Success a wallet which contains a TickRange proposition, consumes input transaction_1
        wallet4 = createWallet(Map(TickRangeAddress -> TickRangeLock), iotx_B)
        iotx_O <- createTransaction_1_3(wallet4, 1L).toResource
        _      <- node1Client.broadcastTransaction(iotx_O).toResource

        // Begin log assertions, improve this with stats https://topl.atlassian.net/browse/BN-777
        logs <- node1.containerLogs[F].through(fs2.text.utf8.decode).through(fs2.text.lines).compile.string.toResource

        // format: off
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_A.id.show} reasons=NonEmptyChain(InsufficientInputFunds)"))
        _ = assert(logs.contains(s"INFO  Bifrost.RPC.Server - Processed Transaction id=${iotx_B.id.show} from RPC"))
        _ = assert(logs.contains(s"INFO  Bifrost.RPC.Server - Received duplicate transaction id=${iotx_B.id.show}")) // ask if duplicate transaction level message should be WARN
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_D.id.show} reasons=NonEmptyChain(EmptyInputs)"))
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_E.id.show} reasons=NonEmptyChain(DuplicateInput(boxId="))
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_F.id.show} reasons=NonEmptyChain(ExcessiveOutputsCount, InvalidDataLength)"))
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_G.id.show} reasons=NonEmptyChain(InvalidTimestamp(timestamp=-1))"))
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_I.id.show} reasons=NonEmptyChain(InvalidSchedule(creation=0,maximumSlot=0,minimumSlot=-1))"))
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_J.id.show} reasons=NonEmptyChain(InvalidSchedule(creation=0,maximumSlot=0,minimumSlot=1))"))
        // Fix this warning message: https://topl.atlassian.net/browse/TSDK-516
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_K.id.show} reasons=NonEmptyChain(NonPositiveOutputValue"))
        _ = iotxs.foreach{iotx => assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx.id.show} reasons=NonEmptyChain(InvalidProofType)"))}
        _ = assert(logs.contains(s"INFO  Bifrost.RPC.Server - Processed Transaction id=${iotx_N.id.show} from RPC"))
        _ = assert(logs.contains(s"INFO  Bifrost.RPC.Server - Processed Transaction id=${iotx_M.id.show} from RPC"))
        _ = assert(logs.contains(s"INFO  Bifrost.RPC.Server - Processed Transaction id=${iotx_O.id.show} from RPC"))
        // format: on

      } yield ()

    resource.use_
  }

  /**
   * Create a wallet
   * @param propositions
   * @param ioTransaction
   * @return a wallet with spendable boxes populated with provided ioTransaction and propositions
   */
  private def createWallet(propositions: Map[LockAddress, Lock], ioTransaction: IoTransaction): Wallet = {
    val newBoxes = ioTransaction.outputs.zipWithIndex.flatMap { case (output, index) =>
      propositions
        .get(output.address)
        .map(lock => (ioTransaction.id.outputAddress(0, 0, index), Box(lock, output.value)))
    }
    Wallet(spendableBoxes = newBoxes.toMap, propositions)
  }

  /**
   * Transacion used to test the following validations
   * - Received syntactically invalid transaction
   * - Processed Transaction
   * - Received duplicate transaction
   *
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction with n outputs
   *  - output 1: HeightRangeAddress
   *  - output 2: DigestAddress
   *  - output 3: DigitalSignature
   *  - output 4: TickRange
   *  - output n TODO: ExactMatch, LessThan, GreaterThan, EqualTo, Threshold, Not, And, or
   */
  private def createTransaction_1(
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt,
    lvlQuantityOutput2: BigInt,
    lvlQuantityOutput3: BigInt,
    lvlQuantityOutput4: BigInt
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
        UnspentTransactionOutput(HeightRangeAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput1))),
        UnspentTransactionOutput(DigestAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput2))),
        UnspentTransactionOutput(DigitalSignatureAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput3))),
        UnspentTransactionOutput(TickRangeAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput4)))
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
   * Transaction_1 -> (Transaction_1_1, Transaction_1_2, Transaction_1_3)
   */
  private def createTransaction_1_1(
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt
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
        UnspentTransactionOutput(DigestAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput1)))
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

      proof <- Prover.digestProver[F].prove(preimage, unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transaction_1 -> (Transaction_1_1, Transaction_1_2, Transaction_1_3)
   */
  private def createTransaction_1_2(
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt
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
        UnspentTransactionOutput(DigitalSignatureAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput1)))
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

      witness = Witness(
        ByteString.copyFrom(
          (new ExtendedEd25519)
            .sign(WalletApi.pbKeyPairToCryotoKeyPair(childKeyPair).signingKey, "transaction binding".getBytes)
        )
      )
      proof <- Prover.signatureProver[F].prove(witness, unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transaction_1 -> (Transaction_1_1, Transaction_1_2, Transaction_1_3)
   */
  private def createTransaction_1_3(
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt
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
          TickRangeAddress,
          Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput1))
        ) // cambie aca
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

      proof <- Prover.tickProver[F].prove((), unprovenTransaction.signable) // cambie aca
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Verify that this transaction does not spend the same box more than once
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction
   */
  private def createTransaction_2(wallet: Wallet): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant
      (inputBoxId, box) = (wallet.spendableBoxes.head._1, wallet.spendableBoxes.head._2)

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

  /**
   * Transacion used to test the following validations
   * - Verify that this transaction does not contain too many outputs.
   * - DataLengthValidation validates approved transaction data length, includes proofs
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction wich is expected to fail
   */
  private def createTransaction_3(wallet: Wallet): F[IoTransaction] =
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

      // too many outputs
      outputs = (1 to Short.MaxValue).map(_ =>
        UnspentTransactionOutput(
          HeightRangeAddress,
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

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Verify that the timestamp of the transaction is positive (greater than 0).
   * - Verify that the schedule of the timestamp contains valid minimum and maximum slot values
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction wich is expected to fail
   */
  private def createTransaction_4(wallet: Wallet, min: Long, max: Long, timestamp: Long): F[IoTransaction] =
    for {
      (inputBoxId, box) <- (wallet.spendableBoxes.head._1, wallet.spendableBoxes.head._2).pure[F]

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
          HeightRangeAddress,
          Value.defaultInstance.withLvl(Value.LVL(BigInt(1)))
        )
      )

      // values provided in the input will fail
      unprovenTransaction =
        IoTransaction.defaultInstance
          .withInputs(inputs)
          .withOutputs(outputs)
          .withDatum(
            Datum.IoTransaction(Event.IoTransaction(Schedule(min, max, timestamp), SmallData.defaultInstance))
          )

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation
   * @param wallet populated wallet with spendableBoxes
   * @prover one of lockedProved, digestProver, signatureProver ... default to heightProver
   * @return a transaction wich is expected to fail
   */
  private def createTransaction_5(wallet: Wallet, prover: String, lvlQuantityOutput1: BigInt): F[IoTransaction] =
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
          HeightRangeAddress,
          Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput1))
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
        prover match { // replace with PropositionType bramblsc
          case "lockedProver" => Prover.lockedProver[F].prove((), unprovenTransaction.signable)
          case "digestProver" =>
            Prover.digestProver[F].prove(Preimage(), unprovenTransaction.signable)
          case "signatureProver" =>
            Prover
              .signatureProver[F]
              .prove(Witness(ByteString.copyFrom(Array.fill(64)(0.byteValue))), unprovenTransaction.signable)
          case "tickProver"        => Prover.tickProver[F].prove((), unprovenTransaction.signable)
          case "exactMatchProver"  => Prover.exactMatchProver[F].prove((), unprovenTransaction.signable)
          case "lessThanProver"    => Prover.lessThanProver[F].prove((), unprovenTransaction.signable)
          case "greaterThanProver" => Prover.greaterThanProver[F].prove((), unprovenTransaction.signable)
          case "equalToProver"     => Prover.equalToProver[F].prove((), unprovenTransaction.signable)
          case "thresholdProver"   => Prover.thresholdProver[F].prove(Set.empty, unprovenTransaction.signable)
          case "notProver"         => Prover.notProver[F].prove(Proof(), unprovenTransaction.signable)
          case "andProver"         => Prover.andProver[F].prove(Proof() -> Proof(), unprovenTransaction.signable)
          case "orProver"          => Prover.orProver[F].prove(Proof() -> Proof(), unprovenTransaction.signable)
          case _                   => Prover.heightProver[F].prove((), unprovenTransaction.signable)
        }

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
