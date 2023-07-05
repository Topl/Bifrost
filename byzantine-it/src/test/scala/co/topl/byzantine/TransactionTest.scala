package co.topl.byzantine

import cats.data.OptionT
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.blockchain.{PrivateTestnet, StakerInitializers}
import co.topl.brambl.builders.locks.PropositionTemplate
import co.topl.brambl.builders.locks.PropositionTemplate.PropositionType
import co.topl.brambl.common.ContainsImmutable.ContainsImmutableTOps
import co.topl.brambl.common.ContainsImmutable.instances.arrayByteImmutable
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances.{immutableSignable, ioTransactionSignable}
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models._
import co.topl.brambl.syntax.{ioTransactionAsTransactionSyntaxOps, lockAsLockSyntaxOps, transactionIdAsIdSyntaxOps}
import co.topl.brambl.wallet.WalletApi
import co.topl.byzantine.util._
import co.topl.consensus.models.{StakingAddress, StakingRegistration}
import co.topl.crypto.generation.Bip32Indexes
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, ExtendedEd25519, KesProduct}
import co.topl.interpreters.NodeRpcOps._
import co.topl.numerics.implicits._
import co.topl.quivr.api.Prover
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import com.spotify.docker.client.DockerClient
import fs2.Stream
import munit.{FailSuiteException, Location}
import org.typelevel.log4cats.Logger
import quivr.models.Proposition.{DigitalSignature, TickRange}
import quivr.models._
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.Random

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

  private val HeightRangeLockAddress =
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

  private val DigestLockAddress =
    DigestLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // Digest END

  // Digital Signature BEGIN: TODO this is not working as expected
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

  private val digitalSignature = DigitalSignature("ExtendedEd25519", childKeyPair.vk)
  private val DigitalSignatureProposition = Proposition(Proposition.Value.DigitalSignature(digitalSignature))

  private val DigitalSignatureLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(DigitalSignatureProposition)), 1))
  )

  private val DigitalSignatureLockAddress =
    DigitalSignatureLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // Digital Signature END

  // TickRange BEGIN
  private val tickRange = TickRange(min = 0L, max = 500L)
  private val badTickRange = TickRange(min = 0L, max = 1L)
  private val TickRangeProposition = Proposition(Proposition.Value.TickRange(tickRange))
  private val BadTickRangeProposition = Proposition(Proposition.Value.TickRange(badTickRange))

  private val TickRangeLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(TickRangeProposition)), 1))
  )

  private val BadTickRangeLock = Lock(
    Lock.Value.Predicate(Lock.Predicate(List(Challenge().withRevealed(BadTickRangeProposition)), 1))
  )

  private val GoodAndBadTickRangeLock = Lock(
    Lock.Value.Predicate(
      Lock.Predicate(
        List(
          Challenge().withRevealed(TickRangeProposition),
          Challenge().withRevealed(BadTickRangeProposition)
        ),
        2
      )
    )
  )

  private val TickRangeLockAddress =
    TickRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  private val BadTickRangeLockAddress =
    BadTickRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  private val GoodAndBadTickRangeLockAddress =
    GoodAndBadTickRangeLock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
  // TickRange END

  // StakingAddress Begin
  private val (kesSK0, _) = new KesProduct().createKeyPair(Random.nextBytes(32), (9, 9), 0L)
  private val vk = ByteString.copyFrom(Random.nextBytes(32))
  private val (vrfSecretBytes, _) = Ed25519VRF.precomputed().generateRandom
  private val stakingAddress = StakingAddress(vk)
  private val vkVrf = ByteString.copyFrom(Ed25519VRF.precomputed().getVerificationKey(vrfSecretBytes))
  private val commitmentMessage = new Blake2b256().hash(vkVrf.concat(stakingAddress.value).toByteArray)
  private val signatureKesProduct = (new KesProduct).sign(kesSK0, commitmentMessage)
  private val stakingRegistration = StakingRegistration(stakingAddress, signatureKesProduct)
  // StakingAddress End

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
   * M. FIX: Success, create and broadcast a txio using a wallet which contains a DigitalSignature proposition, and consumes input 'transaction_1' (step B)
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

        // a wallet populated with the genesis iotx, safe head, it contains 1 transaction, with 0 input and 2 outputs(10000000 TOPL and 10000000 LVL)
        wallet = createWallet(
          Map(HeightRangeLockAddress -> HeightRangeLock),
          // blockResponse.block.fullBody.transactions(0).outputs(0).value.toProtoString).toResource 10000000 TOPL
          // blockResponse.block.fullBody.transactions(0).outputs(1).value.toProtoString).toResource 10000000 LVL
          blockResponse.block.fullBody.transactions.head
        )

        // A. No Success is expected, InsufficientInputFunds create tx from genesis block using: (10000000 lvl in total)
        iotx_A <- createTransaction_1(wallet, 10000000, 1, 1, 1, 1, 1).toResource
        _      <- node1Client.broadcastTransaction(iotx_A).voidError.toResource

        /**
         * B. Success create tx from genesis block using HeightLock: (10000000 -> (9999900, 1,1,1,1,1,1,1))
         *
         * Output1: HeightRangeLockAddress, 9999900 lvl
         * Output2: DigestLockAddress, 1 lvl, with threshold 1
         * Output3: DigitalSignature, 1 lvl, with threshold 1
         * Output4: TickRange, 1 lvl, with threshold 1
         * Output5: BadTickRange, 1 lvl, with threshold 1
         * Output6: GoodAndBadTickRange, 1 lvl, with threshold 2
         */
        iotx_B <- createTransaction_1(wallet, 9999900, 1, 1, 1, 1, 1).toResource
        _      <- node1Client.broadcastTransaction(iotx_B).toResource

        iotxRes_1 <- OptionT(node1Client.fetchTransaction(iotx_B.id))
          .getOrRaise(new FailSuiteException("ERROR! fetchTransaction 1", Location.empty))
          .toResource
        _ <- assertIO((iotxRes_1.outputs(0).value.getLvl.quantity: BigInt).pure[F], BigInt(9999900)).toResource
        _ <- assertIO((iotxRes_1.outputs(1).value.getLvl.quantity: BigInt).pure[F], BigInt(1)).toResource
        _ <- assertIO((iotxRes_1.outputs(2).value.getLvl.quantity: BigInt).pure[F], BigInt(1)).toResource
        _ <- assertIO((iotxRes_1.outputs(3).value.getLvl.quantity: BigInt).pure[F], BigInt(1)).toResource
        _ <- assertIO((iotxRes_1.outputs(4).value.getLvl.quantity: BigInt).pure[F], BigInt(1)).toResource
        _ <- assertIO((iotxRes_1.outputs(5).value.getLvl.quantity: BigInt).pure[F], BigInt(1)).toResource

        // C. No Success, Create the same tx from genesis block using , Received duplicate transaction is expected
        _ <- createTransaction_1(wallet, 9999900, 1, 1, 1, 1, 1).toResource
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
        iotx_K <- createTransaction_1(wallet, -1L, 1L, 1L, 1L, 1L, 1L).toResource
        _      <- node1Client.broadcastTransaction(iotx_K).voidError.toResource

        // L No Success, Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation
        // No success reason, the wallet used only contains a HeightRange proposition
        iotxs <- Async[F]
          .parSequenceN(1)(
            Seq(
              PropositionTemplate.types.Locked    -> 1,
              PropositionTemplate.types.Digest    -> 2,
              PropositionTemplate.types.Signature -> 3,
              PropositionTemplate.types.Tick      -> 4,
//              "exactMatchProver"  -> 5,
//              "lessThanProver"    -> 6,
//              "greaterThanProver" -> 7,
//              "equalToProver"     -> 8,
              PropositionTemplate.types.Threshold -> 9,
              PropositionTemplate.types.Not       -> 10,
              PropositionTemplate.types.And       -> 11,
              PropositionTemplate.types.Or        -> 12
            )
              .map { case (prover, i) => createTransaction_5(wallet, prover, i) }
          )
          .toResource

        _ <- Async[F]
          .parSequenceN(1)(iotxs.map(iotx => node1Client.broadcastTransaction(iotx).voidError))
          .void
          .toResource

        // N. Success a wallet which contains a Digest proposition, consumes input  transaction_1
        iotx_N <- createTransaction_1_1_DigestProver(
          createWallet(Map(DigestLockAddress -> DigestLock), iotx_B),
          1L
        ).toResource
        _ <- node1Client.broadcastTransaction(iotx_N).toResource

        // M. Success a wallet which contains a DigitalSignature proposition, consumes input  transaction_1
        iotx_M <- createTransaction_1_2_SignatureProver(
          createWallet(Map(DigitalSignatureLockAddress -> DigitalSignatureLock), iotx_B),
          1L
        ).toResource
        _ <- node1Client.broadcastTransaction(iotx_M).toResource

        // O. Test Bad and Success iotx with a wallet which contains a Bad and good TickRange proposition, consumes input transaction_1
        iotx_O <- createTransaction_1_3_TickProver(
          createWallet(Map(BadTickRangeLockAddress -> BadTickRangeLock), iotx_B),
          1L
        ).toResource
        _ <- node1Client.broadcastTransaction(iotx_O).toResource

        iotx_P <- createTransaction_1_3_TickProver(
          createWallet(Map(GoodAndBadTickRangeLockAddress -> GoodAndBadTickRangeLock), iotx_B),
          1L
        ).toResource
        _ <- node1Client.broadcastTransaction(iotx_P).toResource

        iotx_Q <- createTransaction_1_3_TickProver(
          createWallet(Map(TickRangeLockAddress -> TickRangeLock), iotx_B),
          1L
        ).toResource
        _ <- node1Client.broadcastTransaction(iotx_Q).toResource

        // TODO continue with Step R, which consumes Topl from genesis tx
        // R. create a transaction which consumes TOPLs, TODO continue later, issues with stakingOperator

        //        genesisIotx = blockResponse.block.fullBody.transactions.head
        //        wallet = Wallet(
        //          spendableBoxes = Map(
        //            genesisIotx.id.outputAddress(0, 0, 1) -> Box(HeightRangeLock, genesisIotx.outputs(1).value) // 10000000 LVL
        //          ),
        //          propositions = Map(
        //            HeightRangeLockAddress -> HeightRangeLock
        //          )
        //        )

        //        stakingOperator = PrivateTestnet
        //          .stakerInitializers(node1.config.bigBangTimestamp.toEpochMilli, stakerCount = 1)
        //          .head
        //        walletWithTopl = Wallet(
        //          spendableBoxes = Map(
        //            genesisIotx.id.outputAddress(0, 0, 0) -> Box(
        //              stakingOperator.lock,
        //              genesisIotx.outputs(0).value
        //            ) // 10000000 TOPL
        //          ),
        //          propositions = Map(stakingOperator.lockAddress -> stakingOperator.lock)
        //        )
        //        iotx_R <- createTransaction_TOPL(walletWithTopl, 1, stakingOperator).toResource
        //        _      <- node1Client.broadcastTransaction(iotx_R).voidError.toResource

        _ <- Async[F].sleep(15.seconds).toResource

        // Begin log assertions, improve this with stats https://topl.atlassian.net/browse/BN-777
        logs <- node1.containerLogs[F].through(fs2.text.utf8.decode).through(fs2.text.lines).compile.string.toResource

        // format: off
        _ = assert(logs.contains(s"WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=${iotx_A.id.show} reasons=NonEmptyChain(InsufficientInputFunds)"))
        _ = assert(logs.contains(s"INFO  Bifrost.RPC.Server - Processed Transaction id=${iotx_B.id.show} from RPC"))
        _ = assert(logs.contains(s"INFO  Bifrost.RPC.Server - Received duplicate transaction id=${iotx_B.id.show}")) // Fix, duplicate transaction level message should be WARN?
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
        _ = assert(logs.contains(s"EvaluationAuthorizationFailed(Proposition(TickRange(TickRange(0,1")) // step O bad tickRange, Fix message
        _ = assert(logs.contains(s"failed authorization validation: AuthorizationFailed(List())")) // step P good and bad tickRange, with threshold 2, Fix message
        _ = assert(logs.contains(s"INFO  Bifrost.RPC.Server - Processed Transaction id=${iotx_Q.id.show} from RPC"))
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

  private def createTransaction_TOPL(
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

      // TODO
      outputs = List(
        UnspentTransactionOutput(
          stakingOperator.lockAddress,
          Value.defaultInstance.withTopl(
            Value.TOPL.defaultInstance
              .withQuantity(lvlQuantityOutput1)
              .withRegistration(stakingRegistration)
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

      // FIX me, how to create a witness?
      fakeMsgBind: SignableBytes = unprovenTransaction.signable // TODO
      witness = Witness(
        ByteString.copyFrom(
          (new Ed25519)
            .sign(Ed25519.SecretKey(stakingOperator.spendingVK.toByteArray), fakeMsgBind.value.toByteArray)
        )
      )
      proof <- Prover.signatureProver[F].prove(witness, unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

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
   *  - output n TODO: ExactMatch, LessThan, GreaterThan, EqualTo, Threshold, Not, And, or
   */
  private def createTransaction_1(
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt,
    lvlQuantityOutput2: BigInt,
    lvlQuantityOutput3: BigInt,
    lvlQuantityOutput4: BigInt,
    lvlQuantityOutput5: BigInt,
    lvlQuantityOutput6: BigInt
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
        UnspentTransactionOutput(HeightRangeLockAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput1))),
        UnspentTransactionOutput(DigestLockAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput2))),
        UnspentTransactionOutput(
          DigitalSignatureLockAddress,
          Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput3))
        ),
        UnspentTransactionOutput(TickRangeLockAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput4))),
        UnspentTransactionOutput(BadTickRangeLockAddress, Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput5))),
        UnspentTransactionOutput(
          GoodAndBadTickRangeLockAddress,
          Value.defaultInstance.withLvl(Value.LVL(lvlQuantityOutput6))
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
   * Transaction_1 -> (Transaction_1_1, Transaction_1_2, Transaction_1_3)
   */
  private def createTransaction_1_1_DigestProver(
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant
      (inputBoxId, box) = (wallet.spendableBoxes.head._1, wallet.spendableBoxes.head._2)
      lockAddress = wallet.propositions.head._1

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

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

      proof <- Prover.digestProver[F].prove(preimage, unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transaction_1 -> (Transaction_1_1, Transaction_1_2, Transaction_1_3)
   */
  private def createTransaction_1_2_SignatureProver(
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant
      (inputBoxId, box) = (wallet.spendableBoxes.head._1, wallet.spendableBoxes.head._2)
      lockAddress = wallet.propositions.head._1

      predicate = Attestation.Predicate(box.lock.getPredicate, Nil)

      inputs = List(
        SpentTransactionOutput(
          address = inputBoxId,
          attestation = Attestation(Attestation.Value.Predicate(predicate)),
          value = box.value
        )
      )

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

      witness = Witness(
        ByteString.copyFrom(
          (new ExtendedEd25519)
            .sign(
              WalletApi.pbKeyPairToCryotoKeyPair(childKeyPair).signingKey,
              unprovenTransaction.signable.value.toByteArray
            )
        )
      )
      proof <- Prover.signatureProver[F].prove(witness, unprovenTransaction.signable)
      predicateWithProof = predicate.copy(responses = List(proof))
      iotx = proveTransaction(unprovenTransaction, predicateWithProof)
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Transaction with good TickRange Proposition
   * - EvaluationAuthorizationFailed(Proposition(TickRange
   */
  private def createTransaction_1_3_TickProver(
    wallet:             Wallet,
    lvlQuantityOutput1: BigInt
  ): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant
      (inputBoxId, box) = (wallet.spendableBoxes.head._1, wallet.spendableBoxes.head._2)
      lockAddress = wallet.propositions.head._1

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
          lockAddress,
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

      proof <- Prover.tickProver[F].prove((), unprovenTransaction.signable)
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

  private def createTransaction_withFailures(
    wallet:  Wallet,
    outputs: Seq[UnspentTransactionOutput],
    datum:   Datum.IoTransaction
  ): F[IoTransaction] =
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
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction wich is expected to fail
   */
  private def createTransaction_3(wallet: Wallet): F[IoTransaction] =
    for {
      timestamp <- Async[F].realTimeInstant
      iotx <- createTransaction_withFailures(
        wallet,
        // too many outputs
        outputs = (1 to Short.MaxValue).map(_ =>
          UnspentTransactionOutput(
            HeightRangeLockAddress,
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
   * @param wallet populated wallet with spendableBoxes
   * @return a transaction wich is expected to fail
   */
  private def createTransaction_4(wallet: Wallet, min: Long, max: Long, timestamp: Long): F[IoTransaction] =
    for {
      iotx <- createTransaction_withFailures(
        wallet,
        outputs = List(
          UnspentTransactionOutput(
            HeightRangeLockAddress,
            Value.defaultInstance.withLvl(Value.LVL(BigInt(1)))
          )
        ),
        Datum.IoTransaction(Event.IoTransaction(Schedule(min, max, timestamp), SmallData.defaultInstance))
      )
    } yield iotx

  /**
   * Transacion used to test the following validations
   * - Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation
   * @param wallet populated wallet with spendableBoxes
   * @prover one of lockedProved, digestProver, signatureProver ... default to heightProver
   * @return a transaction wich is expected to fail
   */
  private def createTransaction_5(
    wallet:             Wallet,
    prover:             PropositionType,
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
          HeightRangeLockAddress,
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
          case PropositionTemplate.types.Locked => Prover.lockedProver[F].prove((), unprovenTransaction.signable)
          case PropositionTemplate.types.Digest =>
            Prover.digestProver[F].prove(Preimage(), unprovenTransaction.signable)
          case PropositionTemplate.types.Signature =>
            Prover
              .signatureProver[F]
              .prove(Witness(ByteString.copyFrom(Array.fill(64)(0.byteValue))), unprovenTransaction.signable)
          case PropositionTemplate.types.Tick => Prover.tickProver[F].prove((), unprovenTransaction.signable)
          // TODO there is no proposition template for the following Prover
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
