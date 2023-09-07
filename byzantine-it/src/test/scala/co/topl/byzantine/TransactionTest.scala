package co.topl.byzantine

import cats.data.OptionT
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.blockchain.PrivateTestnet
import co.topl.brambl.builders.locks.PropositionTemplate
import co.topl.brambl.models._
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.{ioTransactionAsTransactionSyntaxOps, transactionIdAsIdSyntaxOps}
import co.topl.byzantine.transactions.{Locks, TransactionFactory, Wallet}
import co.topl.byzantine.util._
import co.topl.interpreters.NodeRpcOps._
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._
import com.spotify.docker.client.DockerClient
import fs2.Stream
import munit.{FailSuiteException, Location}
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.{Duration, DurationInt}

class TransactionTest extends IntegrationSuite {

  type RpcClient = NodeRpc[F, Stream[F, *]]

  override val munitTimeout: Duration = 3.minutes

  /**
   * Submits many transactions to a group of running node and verifies the transactions are confirmed
   * The following step are being evaluated: No success meaning: broadcast the iotx to the node, produce an error.
   * A. InsufficientInputFunds, if we use a iotx which spent more items than expected from a box, using genesis iotx
   * B. Success, produces 'iotx_B', uses genesis txio, produces N outputs, described on step B
   * C. Received duplicate transaction, if we try to do the same that step B
   * D. Verify that submit a transaction contains at least one input
   * E. Verify that this transaction does not spend the same box more than once, No Success is expected
   * F. Verify that this transaction contain too many outputs, ExcessiveOutputsCount, DataLengthValidation, No Success is expected
   * G. Verify that the timestamp of the transaction is positive (greater than or equal to 0). No Success is expected
   * H. Verify that the schedule of the timestamp contains valid minimum and maximum slot values. No Success is expected
   * I. Verify that the schedule of the timestamp contains valid minimum and maximum slot values. No Success is expected
   * J. Verify again that the schedule of the timestamp contains valid minimum and maximum slot values. No Success is expected
   * K. Verify that each transaction output contains a positive quantity (where applicable). No Success is expected
   * L. Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation, No Success is expected
   * N. Success, create and broadcast a txio using a wallet which contains a Digest proposition, and consumes input 'iotx_B' (step B)
   * M. Success, create and broadcast a txio using a wallet which contains a DigitalSignature proposition, and consumes input 'iotx_B' (step B)
   * O. Success, create and broadcast a txio using a wallet which contains a TickRange proposition, and consumes input 'iotx_B' (step B)
   * P. Failure iotx with a wallet which contains a Bad and good TickRange proposition, consumes input iotx_B
   * Q. Success iotx with a wallet which contains a good TickRange proposition, consumes input iotx_B
   * Q_and. Failure, same than P, but using And proposition, consumes input transaction_1
   * Q_not. Failure, same than P, but using not proposition, consumes input transaction_1
   * Q_or. Success, same than P, but using or proposition, consumes input transaction_1
   * R. Success. create and broadcast a txio using a wallet which contains a HighLock proposition, and consumes input 'genesisToplTx' (step B)
   * S. Success. create Group Constructor and broadcast a txio using a wallet which contains a HighLock proposition, consumes input 'genesisToplTx' (step B)
   * T. Failure. create a invalid Group Constructor and broadcast a txio using a wallet which contains a HighLock proposition, consumes input 'genesisToplTx' (step B)
   * ....
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
        // blockResponse transactions outputs(10000000 TOPL and 10000000 LVL)
        genesisToplTx = blockResponse.block.fullBody.transactions(0) // 10000000 TOPL
        genesisLvlTx = blockResponse.block.fullBody.transactions(1) // 10000000 LVL

        _ <- Logger[F]
          .info(show"blockResponse height 1 output size=${blockResponse.block.fullBody.transactions.size}")
          .toResource

        // genesisToplTx quantity=10000000
        _ <- Logger[F]
          .info(show"genesisToplTx quantity=${genesisToplTx.outputs.head.value.getTopl.quantity: BigInt}")
          .toResource
        // genesisLvlTx quantity=10000000
        _ <- Logger[F]
          .info(show"genesisLvlTx  quantity=${genesisLvlTx.outputs.head.value.getLvl.quantity: BigInt}")
          .toResource

        // a box populated with the genesis iotx Lvls
        inputBoxId = genesisLvlTx.id.outputAddress(0, 0, 0)
        box = Box(Locks.HeightRangeLock, genesisLvlTx.outputs.head.value) // 10000000 LVL

        // A. No Success is expected, InsufficientInputFunds create tx from genesis block using: (10000000 lvl in total)
        iotx_A <- TransactionFactory
          .createTransaction_fromGenesisIotx[F](inputBoxId, box, lvlQuantity1 = 10000000)
          .toResource
        _ <- Logger[F].info(show"Failure iotx_A ${iotx_A.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_A).voidError.toResource

        /**
         * B. Success create tx from genesis block using HeightLock: (10000000 -> (9999900, 1,1,1,1,1,1,1))
         *
         * Output1: HeightRange Lock, 9999900 lvl
         * Output2: Digest Lock, 1 lvl, with threshold 1
         * Output3: Digital Signature Lock, 1 lvl, with threshold 1
         * Output4: TickRange Lock, 1 lvl, with threshold 1
         * Output5: BadTickRange Lock, 1 lvl, with threshold 1
         * Output7: GoodAndBadTickRange Lock, 1 lvl, with threshold 2,
         * Output7: GoodTickRangeNot Lock, 1 lvl, with threshold 1
         * Output8: GoodBadTickRangeOr Lock, 1 lvl, with threshold 1
         * Output9: HeightRange Lock, 1 lvl, with threshold 1, will be consumed on step S, with a group construtor
         * Output10: HeightRange Lock, 1 lvl, with threshold 1, will be consumed on step T, with a series construtor
         * Output11: HeightRange Lock, 1 lvl, with threshold 1, will be consumed on step U, with a group construtor
         * Output12: HeightRange Lock, 1 lvl, with threshold 1, will be consumed on step U, with a series construtor
         */
        iotx_B <- TransactionFactory
          .createTransaction_fromGenesisIotx[F](inputBoxId, box, lvlQuantity1 = 9999900)
          .toResource
        _ <- Logger[F].info(show"Success iotx_B ${iotx_B.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_B).toResource

        iotxRes_B <- OptionT(node1Client.fetchTransaction(iotx_B.id)).getOrRaise(failSuiteException("B")).toResource

        _ = assertEquals(iotxRes_B.outputs(0).value.getLvl.quantity: BigInt, BigInt(9999900))
        _ = (1 to 12).map(i => assertEquals(iotxRes_B.outputs(i).value.getLvl.quantity: BigInt, BigInt(1)))

        // C. No Success, Create the same tx from genesis block, Received duplicate transaction is expected
        _ <- TransactionFactory
          .createTransaction_fromGenesisIotx[F](inputBoxId, box, lvlQuantity1 = 9999900L)
          .toResource
        _ <- node1Client.broadcastTransaction(iotx_B).toResource

        // D. No Success, Verify that this transaction contains at least one input
        iotx_D <- IoTransaction.defaultInstance.pure[F].toResource
        _      <- Logger[F].info(show"Failure iotx_D ${iotx_D.id.show}").toResource
        _      <- node1Client.broadcastTransaction(iotx_D).voidError.toResource

        // E. No Success, Verify that a transaction does not spend the same box more than once
        iotx_E <- TransactionFactory.createTransaction_DoubleSpend[F](inputBoxId, box).toResource
        _      <- Logger[F].info(show"Failure iotx_E ${iotx_E.id.show}").toResource
        _      <- node1Client.broadcastTransaction(iotx_E).voidError.toResource

        // F. No Success, Verify that a transaction contain too many outputs, ExcessiveOutputsCount, DataLengthValidation
        iotx_F <- TransactionFactory.createTransaction_ManyOutputs[F](inputBoxId, box).toResource
        _      <- Logger[F].info(show"Failure iotx_F ${iotx_F.id.show}").toResource
        _      <- node1Client.broadcastTransaction(iotx_F).voidError.toResource

        // G. No Success, Verify that the timestamp of the transaction is positive (greater than or equal to 0). Bad timestamp
        iotx_G <- TransactionFactory
          .createTransaction_BadSchedule[F](inputBoxId, box, min = 0L, max = 0L, timestamp = -1L)
          .toResource
        _ <- Logger[F].info(show"Failure iotx_G ${iotx_G.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_G).voidError.toResource

        // H. No Success, Verify that the schedule of the timestamp contains valid minimum and maximum slot values. Bad min
        iotx_H <- TransactionFactory
          .createTransaction_BadSchedule[F](inputBoxId, box, min = 1L, max = 0L, timestamp = 0L)
          .toResource
        _ <- Logger[F].info(show"Failure iotx_H ${iotx_H.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_H).voidError.toResource

        // I. No Success, Verify that the schedule of the timestamp contains valid minimum and maximum slot values Bad min,max
        iotx_I <- TransactionFactory
          .createTransaction_BadSchedule[F](inputBoxId, box, min = -1L, max = 0L, timestamp = 0L)
          .toResource
        _ <- Logger[F].info(show"Failure iotx_I ${iotx_I.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_I).voidError.toResource

        // J. No Success, Verify that the schedule of the timestamp contains valid minimum and maximum slot values Bad min,max
        iotx_J <- TransactionFactory
          .createTransaction_BadSchedule[F](inputBoxId, box, min = 1L, max = 0L, timestamp = 0L)
          .toResource
        _ <- Logger[F].info(show"Failure iotx_J ${iotx_J.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_J).voidError.toResource

        // K. No Success, Verify that each transaction output contains a positive quantity (where applicable)
        iotx_K <- TransactionFactory
          .createTransaction_fromGenesisIotx[F](inputBoxId, box, lvlQuantity1 = -1L)
          .toResource
        _ <- Logger[F].info(show"Failure iotx_K ${iotx_K.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_K).voidError.toResource

        // L No Success, Validates that the proofs associated with each proposition matches the expected _type_ for a Predicate Attestation
        // No success reason, the wallet used only contains a HeightRange proposition
        iotxs <- Async[F]
          .parSequenceN(1)(
            Seq(
              PropositionTemplate.types.Locked,
              PropositionTemplate.types.Digest,
              PropositionTemplate.types.Signature,
              PropositionTemplate.types.Tick,
//              "exactMatchProver",
//              "lessThanProver",
//              "greaterThanProver",
//              "equalToProver",
              PropositionTemplate.types.Threshold,
              PropositionTemplate.types.Not,
              PropositionTemplate.types.And,
              PropositionTemplate.types.Or
            )
              .map { case (prover) => TransactionFactory.createTransaction_BadAttestation[F](inputBoxId, box, prover) }
          )
          .toResource

        _ <- Async[F]
          .parSequenceN(1)(iotxs.map(iotx => node1Client.broadcastTransaction(iotx).voidError))
          .void
          .toResource

        // N. Success a wallet which contains a Digest proposition, consumes input  transaction_1
        iotx_N <- TransactionFactory
          .createTransaction_usingInput_B[F](
            createWallet(Map(Locks.DigestLockAddress -> Locks.DigestLock), iotx_B),
            1L,
            PropositionTemplate.types.Digest
          )
          .toResource
        _ <- Logger[F].info(show"Success iotx_N ${iotx_N.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_N).toResource

        // M. Success a wallet which contains a DigitalSignature proposition, consumes input  transaction_1
        iotx_M <- TransactionFactory
          .createTransaction_usingInput_B[F](
            createWallet(Map(Locks.DigitalSignatureLockAddress -> Locks.DigitalSignatureLock), iotx_B),
            1L,
            PropositionTemplate.types.Signature
          )
          .toResource
        _ <- Logger[F].info(show"Success iotx_M ${iotx_M.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_M).toResource

        // O. Failure iotx with a wallet which contains a Bad and good TickRange proposition, consumes input transaction_1
        iotx_O <- TransactionFactory
          .createTransaction_usingInput_B[F](
            createWallet(Map(Locks.BadTickRangeLockAddress -> Locks.BadTickRangeLock), iotx_B),
            1L,
            PropositionTemplate.types.Tick
          )
          .toResource
        _ <- Logger[F].info(show"Failure iotx_O ${iotx_O.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_O).toResource

        // P. Failure iotx with a wallet which contains a Bad and good TickRange proposition, consumes input transaction_1
        iotx_P <- TransactionFactory
          .createTransaction_usingInput_B[F](
            createWallet(Map(Locks.GoodAndBadTickRangeLockAddress -> Locks.GoodAndBadTickRangeLock), iotx_B),
            1L,
            PropositionTemplate.types.Tick
          )
          .toResource
        _ <- Logger[F].info(show"Failure iotx_P ${iotx_P.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_P).toResource

        // Q. Success iotx with a wallet which contains and good TickRange proposition, consumes input transaction_1
        iotx_Q <- TransactionFactory
          .createTransaction_usingInput_B[F](
            createWallet(Map(Locks.TickRangeLockAddress -> Locks.TickRangeLock), iotx_B),
            1L,
            PropositionTemplate.types.Tick
          )
          .toResource
        _ <- Logger[F].info(show"Success iotx_Q ${iotx_Q.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_Q).toResource

        // Q with And. Failure
        iotx_Q_And <- TransactionFactory
          .createTransaction_usingInput_B[F](
            createWallet(Map(Locks.GoodBadTickRangeAndLockAddress -> Locks.GoodBadTickRangeAndLock), iotx_B),
            1L,
            PropositionTemplate.types.And
          )
          .toResource
        _ <- Logger[F].info(show"Failure iotx_Q_And ${iotx_Q_And.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_Q_And).toResource

        // here block packer should contain 3 errors

        // Q with Not, Failure
        iotx_Q_Not <- TransactionFactory
          .createTransaction_usingInput_B[F](
            createWallet(Map(Locks.GoodTickRangeNotLockAddress -> Locks.GoodTickRangeNotLock), iotx_B),
            1L,
            PropositionTemplate.types.Not
          )
          .toResource
        _ <- Logger[F].info(show"Failure iotx_Q_Not ${iotx_Q_Not.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_Q_Not).toResource

        // here block packer should contain 4 errors

        // Q with Or, Success
        iotx_Q_Or <- TransactionFactory
          .createTransaction_usingInput_B[F](
            createWallet(Map(Locks.GoodBadTickRangeOrLockAddress -> Locks.GoodBadTickRangeOrLock), iotx_B),
            1L,
            PropositionTemplate.types.Or
          )
          .toResource
        _ <- Logger[F].info(show"Success iotx_Q_Or ${iotx_Q_Or.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_Q_Or).toResource

        // R. Success create a transaction which consumes TOPLs
        walletWithSpendableTopls = Wallet(
          spendableBoxes = Map(
            genesisToplTx.id.outputAddress(0, 0, 0) -> Box(
              PrivateTestnet.HeightLockOneLock,
              genesisToplTx.outputs.head.value
            ) // 10000000 TOPLs
          ),
          propositions = Map(
            PrivateTestnet.HeightLockOneSpendingAddress -> PrivateTestnet.HeightLockOneLock
          )
        )
        stakingOperator = PrivateTestnet.stakerInitializers(
          node1.config.bigBangTimestamp.toEpochMilli,
          node1.config.stakerCount
        )

        iotx_R <- TransactionFactory
          .createTransaction_TOPL[F](walletWithSpendableTopls, 1, stakingOperator.head)
          .toResource
        _         <- Logger[F].info(show"Success iotx_R ${iotx_R.id.show}").toResource
        _         <- node1Client.broadcastTransaction(iotx_R).toResource
        iotxRes_R <- OptionT(node1Client.fetchTransaction(iotx_R.id)).getOrRaise(failSuiteException("R")).toResource
        _         <- assertIO((iotxRes_R.outputs.head.value.getTopl.quantity: BigInt).pure[F], BigInt(1)).toResource

        // S. Success Create a group transaction token , using iotxRes_B lvl Tx
        iotx_S <- TransactionFactory
          .createTransactionGroupTamV2[F](
            inputBoxId = iotxRes_B.id.outputAddress(0, 0, 9),
            Box(Locks.HeightRangeLock, iotxRes_B.outputs(9).value),
            quantity = BigInt(1)
          )
          .toResource

        _         <- Logger[F].info(show"Success iotx_S ${iotx_S.id.show}").toResource
        _         <- node1Client.broadcastTransaction(iotx_S).toResource
        iotxRes_S <- OptionT(node1Client.fetchTransaction(iotx_S.id)).getOrRaise(failSuiteException("S")).toResource
        _         <- assertIO((iotxRes_S.outputs.head.value.getGroup.quantity: BigInt).pure[F], BigInt(1)).toResource

        // T. Success Create a series transaction token , using iotxRes_B lvl Tx
        iotx_T <- TransactionFactory
          .createTransactionSeriesTamV2[F](
            inputBoxId = iotxRes_B.id.outputAddress(0, 0, 10),
            Box(Locks.HeightRangeLock, iotxRes_B.outputs(10).value),
            quantity = BigInt(1)
          )
          .toResource

        _         <- Logger[F].info(show"Success iotx_T ${iotx_T.id.show}").toResource
        _         <- node1Client.broadcastTransaction(iotx_T).toResource
        iotxRes_T <- OptionT(node1Client.fetchTransaction(iotx_T.id)).getOrRaise(failSuiteException("T")).toResource
        _         <- assertIO((iotxRes_T.outputs.head.value.getSeries.quantity: BigInt).pure[F], BigInt(1)).toResource

        // U_Fail_1. Failure, Create a Series and Group constructor token, in the same transaction, same outputAddress wrong
        iotx_U_Fail_1 <- TransactionFactory
          .createTransactionGroupANDSeriesTamV2[F](
            inputBoxIdGroup = iotxRes_B.id.outputAddress(0, 0, 11),
            inputBoxIdSeries = iotxRes_B.id.outputAddress(0, 0, 11),
            lock = Locks.HeightRangeLock, // using the same lock boths
            valueInputGroup = iotxRes_B.outputs(11).value,
            valueInputSeries = iotxRes_B.outputs(12).value,
            groupQuantity = BigInt(1),
            seriesQuantity = BigInt(1)
          )
          .toResource

        _ <- Logger[F].info(show"Failure iotx_U_Fail_1 ${iotx_U_Fail_1.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_U_Fail_1).voidError.toResource

        // U_Success. Create a Series and Group constructor token, in the same transaction, quantities good
        iotx_U <- TransactionFactory
          .createTransactionGroupANDSeriesTamV2[F](
            inputBoxIdGroup = iotxRes_B.id.outputAddress(0, 0, 11),
            inputBoxIdSeries = iotxRes_B.id.outputAddress(0, 0, 12),
            lock = Locks.HeightRangeLock, // using the same lock boths
            valueInputGroup = iotxRes_B.outputs(11).value,
            valueInputSeries = iotxRes_B.outputs(12).value,
            groupQuantity = BigInt(1),
            seriesQuantity = BigInt(1)
          )
          .toResource

        _ <- Logger[F].info(show"Success iotx_U ${iotx_U.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_U).toResource

        // V_Fail_1. Moving a Group constructor token, It was created on step S.
        iotx_V_Fail_1 <- TransactionFactory
          .createTransactionMoveGroupANDSeriesTamV2[F](
            inputBoxIdGroup = iotxRes_S.id.outputAddress(0, 0, 0),
            lock = Locks.HeightRangeLock,
            valueInputGroup = iotxRes_S.outputs.head.value,
            valueOutputGroup = Value.defaultInstance.withGroup(
              Value.Group(
                groupId = iotxRes_S.outputs.head.value.getGroup.groupId,
                quantity = BigInt(2)
              )
            )
          )
          .toResource

        _ <- Logger[F].info(show"Failure iotx_V_Fail_1 ${iotx_V_Fail_1.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_V_Fail_1).voidError.toResource

        // V_Succed. Moving a Group constructor token, It was created on step S.
        iotx_V <- TransactionFactory
          .createTransactionMoveGroupANDSeriesTamV2[F](
            inputBoxIdGroup = iotxRes_S.id.outputAddress(0, 0, 0),
            lock = Locks.HeightRangeLock,
            valueInputGroup = iotxRes_S.outputs.head.value,
            valueOutputGroup = Value.defaultInstance.withGroup(
              Value.Group(
                groupId = iotxRes_S.outputs.head.value.getGroup.groupId,
                quantity = BigInt(1)
              )
            )
          )
          .toResource

        _ <- Logger[F].info(show"Success iotx_V ${iotx_V.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_V).toResource

        // W_Fail_1. Moving a Series constructor token, It was created on step T.
        iotx_W_Fail_1 <- TransactionFactory
          .createTransactionMoveGroupANDSeriesTamV2[F](
            inputBoxIdGroup = iotxRes_T.id.outputAddress(0, 0, 0),
            lock = Locks.HeightRangeLock,
            valueInputGroup = iotxRes_T.outputs.head.value,
            valueOutputGroup = Value.defaultInstance.withSeries(
              Value.Series(
                seriesId = iotxRes_T.outputs.head.value.getSeries.seriesId,
                quantity = BigInt(2)
              )
            )
          )
          .toResource

        _ <- Logger[F].info(show"W_Fail_1 iotx_W_Fail_1 ${iotx_W_Fail_1.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_W_Fail_1).voidError.toResource

        // W_Succed. Moving a Series constructor token, It was created on step T.
        iotx_W <- TransactionFactory
          .createTransactionMoveGroupANDSeriesTamV2[F](
            inputBoxIdGroup = iotxRes_T.id.outputAddress(0, 0, 0),
            lock = Locks.HeightRangeLock,
            valueInputGroup = iotxRes_T.outputs.head.value,
            valueOutputGroup = Value.defaultInstance.withSeries(
              Value.Series(
                seriesId = iotxRes_T.outputs.head.value.getSeries.seriesId,
                quantity = BigInt(1)
              )
            )
          )
          .toResource

        _ <- Logger[F].info(show"iotx_W ${iotx_W.id.show}").toResource
        _ <- node1Client.broadcastTransaction(iotx_W).toResource

        // END of sending transaction, waiting for logs
        _ <- Async[F].sleep(10.seconds).toResource

        processedTransaction <- LogParserTest.processStreamFromRpc(node1.containerLogs[F]).toResource
        _ <- Logger[F].info("PROCESSED").toResource >> Logger[F].info(processedTransaction.mkString(",")).toResource

        _ = assert(processedTransaction.contains(iotx_B.id.show))
        _ = assert(processedTransaction.contains(iotx_N.id.show))
        _ = assert(processedTransaction.contains(iotx_M.id.show))
        _ = assert(processedTransaction.contains(iotx_Q.id.show))
        _ = assert(processedTransaction.contains(iotx_Q_And.id.show))
        _ = assert(processedTransaction.contains(iotx_Q_Not.id.show))
        _ = assert(processedTransaction.contains(iotx_Q_Or.id.show))
        _ = assert(processedTransaction.contains(iotx_R.id.show))
        _ = assert(processedTransaction.contains(iotx_S.id.show))
        _ = assert(processedTransaction.contains(iotx_T.id.show))
        _ = assert(processedTransaction.contains(iotx_V.id.show))

        syntacticallyInvalid <- LogParserTest
          .receivedSyntacticallyInvalidStream(node1.containerLogs[F])
          .toResource

        _ <- Logger[F]
          .info("SYNTACTIC INVALID")
          .toResource >> Logger[F].info(syntacticallyInvalid.mkString(",")).toResource

        _ = assert(syntacticallyInvalid.contains(iotx_A.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_D.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_E.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_F.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_G.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_H.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_I.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_J.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_K.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_U_Fail_1.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_V_Fail_1.id.show))
        _ = assert(syntacticallyInvalid.contains(iotx_W_Fail_1.id.show))

        minted <- LogParserTest.mintedStream(node1.containerLogs[F]).toResource
        mintedSplit = minted.flatMap(s => s.split(",")).filter(_.nonEmpty).map(_.trim)
        _ <- Logger[F].info("MINTED").toResource >> Logger[F].info(mintedSplit.mkString(",")).toResource

        _ = assert(mintedSplit.contains(iotx_B.id.show))
        _ = assert(mintedSplit.contains(iotx_N.id.show))
        _ = assert(mintedSplit.contains(iotx_M.id.show))
        _ = assert(mintedSplit.contains(iotx_Q.id.show))
        _ = assert(mintedSplit.contains(iotx_Q_Or.id.show))
        _ = assert(mintedSplit.contains(iotx_R.id.show))
        _ = assert(mintedSplit.contains(iotx_S.id.show))
        _ = assert(mintedSplit.contains(iotx_T.id.show))
        _ = assert(mintedSplit.contains(iotx_U.id.show))
        _ = assert(mintedSplit.contains(iotx_V.id.show))
        _ = assert(mintedSplit.contains(iotx_W.id.show))

      } yield ()

    resource.use_
  }

  def failSuiteException(name: String) = new FailSuiteException(s"ERROR! fetchTransaction $name", Location.empty)

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
}
