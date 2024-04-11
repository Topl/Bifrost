package co.topl.blockchain

import cats.data.Validated
import cats.effect.IO
import co.topl.brambl.models.{Datum, TransactionId, TransactionOutputAddress}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.{TransactionAuthorizationVerifier, TransactionCostCalculator}
import co.topl.config.ApplicationConfig.Bifrost.MempoolProtection
import co.topl.ledger.algebras._
import co.topl.ledger.models.{IoTransactionEx, MempoolGraph, RewardQuantities}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import cats.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.models.generators.consensus._
import org.scalacheck.{Arbitrary, Gen}
import co.topl.models.ModelGenerators._
import co.topl.networking.fsnetwork.TestHelper.arbitraryIoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.brambl.syntax._
import co.topl.brambl.validation.TransactionAuthorizationError
import co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId
import co.topl.ledger.models._
import co.topl.quivr.runtime.DynamicContext
import org.scalacheck.effect.PropF

class MempoolProtectedTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  private val dummyRewardCalc: TransactionRewardCalculatorAlgebra = (_: IoTransaction) => RewardQuantities()
  private val dummyCostCalc: TransactionCostCalculator = (tx: IoTransaction) => tx.inputs.size

  def arbitraryTx: IoTransaction = Arbitrary(arbitraryIoTransaction.arbitrary.map(_.embedId)).arbitrary.first

  def makeTransaction: (IoTransaction, TransactionId => F[IoTransaction]) = {
    val tx = arbitraryTx
    val fetcher = (_: TransactionId) => tx.pure[F]
    (tx, fetcher)
  }

  def semanticAndAuthCheckOk(
    tx: IoTransaction
  ): (TransactionSemanticValidationAlgebra[F], TransactionAuthorizationVerifier[F]) = {
    val semanticValidationAlgebra = mock[TransactionSemanticValidationAlgebra[F]]
    val transactionAuthorizationVerifier = mock[TransactionAuthorizationVerifier[F]]

    (semanticValidationAlgebra
      .validate(_: TransactionValidationContext)(_: IoTransaction))
      .expects(*, tx)
      .anyNumberOfTimes()
      .returns(
        Validated
          .validNec[TransactionSemanticError, IoTransaction](tx)
          .pure[F]
      )

    (transactionAuthorizationVerifier
      .validate(_: DynamicContext[F, String, Datum])(_: IoTransaction))
      .expects(*, tx)
      .anyNumberOfTimes()
      .returns(
        Either
          .right[TransactionAuthorizationError, IoTransaction](tx)
          .pure[F]
      )

    (semanticValidationAlgebra, transactionAuthorizationVerifier)
  }

  test("Protected memory pool shall just pass request to underlying if protection is disabled") {
    withMock {
      val semanticValidationAlgebra = mock[TransactionSemanticValidationAlgebra[F]]
      val transactionAuthorizationVerifier = mock[TransactionAuthorizationVerifier[F]]

      val transactionRewardCalculator = dummyRewardCalc
      val txCostCalculator = dummyCostCalc

      val boxIdToHeight = mock[TransactionOutputAddress => F[Option[Long]]]
      val config = MempoolProtection(enabled = false)
      val currentBlockHeader = Arbitrary(ModelGenerators.headerGen()).arbitrary.first
      val currentBlockHeaderF = currentBlockHeader.pure[F]

      val underlying = mock[MempoolAlgebra[F]]
      val readBlockId = arbitraryBlockId.arbitrary.first
      val readMempoolGraph = MempoolGraph(Map.empty, Map.empty, Map.empty, dummyRewardCalc, dummyCostCalc)
      (underlying.read _).expects(readBlockId).once().returns(readMempoolGraph.pure[F])

      val removeTx = arbitraryTx
      (underlying.remove _).expects(removeTx.id).returns(().pure[F])

      val containsBlockId = arbitraryBlockId.arbitrary.first
      val containsTx = arbitraryTx
      (underlying.contains _).expects(containsBlockId, containsTx.id).returns(true.pure[F])

      val (addedTx, fetchTransaction) = makeTransaction
      val addMempoolGraph = MempoolGraph(Map.empty, Map.empty, Map.empty, dummyRewardCalc, dummyCostCalc)
      (underlying.read _).expects(currentBlockHeader.id).once().returns(addMempoolGraph.pure[F])
      (underlying.add _).expects(addedTx.id).once().returns(true.pure[F])

      val res =
        for {
          mempool <- MempoolProtected.make[F](
            underlying,
            semanticValidationAlgebra,
            transactionAuthorizationVerifier,
            currentBlockHeaderF,
            fetchTransaction,
            transactionRewardCalculator,
            txCostCalculator,
            boxIdToHeight,
            config
          )

          _ <- mempool.read(readBlockId).toResource
          _ <- mempool.remove(removeTx.id).toResource
          _ <- mempool.contains(containsBlockId, containsTx.id).assertEquals(true).toResource
          _ <- mempool.add(addedTx.id).assertEquals(true).toResource

        } yield ()

      res.use_
    }
  }

  test("Protected memory pool shall not work if threshold doesn't hit / reject invalid semantic tx") {
    withMock {
      val semanticValidationAlgebra = mock[TransactionSemanticValidationAlgebra[F]]
      val transactionAuthorizationVerifier = mock[TransactionAuthorizationVerifier[F]]

      val transactionRewardCalculator = dummyRewardCalc

      val boxIdToHeight = mock[TransactionOutputAddress => F[Option[Long]]]
      val maxMempoolSize = 10000
      val config =
        MempoolProtection(enabled = true, protectionEnabledThresholdPercent = 10, maxMempoolSize = maxMempoolSize)
      val currentBlockHeader = Arbitrary(ModelGenerators.headerGen()).arbitrary.first
      val currentBlockHeaderF = currentBlockHeader.pure[F]

      val underlying = mock[MempoolAlgebra[F]]
      val (addedTx, fetchTransaction) = makeTransaction
      val costMock = mock[TransactionCostCalculator]

      val txInMempool = arbitraryTx
      val txExInMempool = IoTransactionEx(txInMempool, RewardQuantities(), 500L)
      val mempoolGraph =
        MempoolGraph(Map(txInMempool.id -> txExInMempool), Map.empty, Map.empty, dummyRewardCalc, dummyCostCalc)

      // first tx, no check because we didn't hit threshold
      (underlying.read _).expects(currentBlockHeader.id).once().returns(mempoolGraph.pure[F])
      (costMock.costOf _).expects(addedTx).returns(499L)
      (underlying.add _).expects(addedTx.id).once().returns(true.pure[F])

      // second tx, we hit threshold, semantic check will fail
      (underlying.read _).expects(currentBlockHeader.id).once().returns(mempoolGraph.pure[F])
      (costMock.costOf _).expects(addedTx).returns(500L)
      (semanticValidationAlgebra
        .validate(_: TransactionValidationContext)(_: IoTransaction))
        .expects(*, addedTx)
        .once()
        .returns(
          Validated
            .invalidNec[TransactionSemanticError, IoTransaction](
              TransactionSemanticErrors.UnspendableBox(TransactionOutputAddress(0, 0, 0, addedTx.id))
            )
            .pure[F]
        )

      val res =
        for {
          mempool <- MempoolProtected.make[F](
            underlying,
            semanticValidationAlgebra,
            transactionAuthorizationVerifier,
            currentBlockHeaderF,
            fetchTransaction,
            transactionRewardCalculator,
            costMock,
            boxIdToHeight,
            config
          )

          _ <- mempool.add(addedTx.id).assertEquals(true).toResource
          _ <- mempool.add(addedTx.id).assertEquals(false).toResource

        } yield ()

      res.use_
    }
  }

  test("Do not add tx if semantic check is failed / Semantic check shall use different contexts") {
    withMock {
      val semanticValidationAlgebra = mock[TransactionSemanticValidationAlgebra[F]]

      val transactionAuthorizationVerifier = mock[TransactionAuthorizationVerifier[F]]

      val transactionRewardCalculator = dummyRewardCalc

      val boxIdToHeight = mock[TransactionOutputAddress => F[Option[Long]]]
      val maxMempoolSize = 10000
      val config =
        MempoolProtection(
          enabled = true,
          protectionEnabledThresholdPercent = 10,
          useMempoolForSemanticThresholdPercent = 15,
          maxMempoolSize = maxMempoolSize
        )
      val currentBlockHeader = Arbitrary(ModelGenerators.headerGen()).arbitrary.first
      val currentBlockHeaderF = currentBlockHeader.pure[F]

      val underlying = mock[MempoolAlgebra[F]]
      val (addedTx, fetchTransaction) = makeTransaction
      val costMock = mock[TransactionCostCalculator]

      val txInMempool = arbitraryTx
      val txExInMempool = IoTransactionEx(txInMempool, RewardQuantities(), 500L)
      val mempoolGraph =
        MempoolGraph(Map(txInMempool.id -> txExInMempool), Map.empty, Map.empty, dummyRewardCalc, dummyCostCalc)

      // Consider tx already in memorypool because did not hit useMempoolForSemanticThreshold threshold
      (underlying.read _).expects(currentBlockHeader.id).once().returns(mempoolGraph.pure[F])
      (costMock.costOf _).expects(addedTx).returns(500L)

      val useTxInMempoolContext = StaticTransactionValidationContext(
        currentBlockHeader.id,
        Seq(txInMempool),
        currentBlockHeader.height,
        currentBlockHeader.slot
      )

      (semanticValidationAlgebra
        .validate(_: TransactionValidationContext)(_: IoTransaction))
        .expects(useTxInMempoolContext, addedTx)
        .once()
        .returns(
          Validated
            .invalidNec[TransactionSemanticError, IoTransaction](
              TransactionSemanticErrors.UnspendableBox(TransactionOutputAddress(0, 0, 0, addedTx.id))
            )
            .pure[F]
        )

      // Do not consider tx already in memorypool because hit useMempoolForSemanticThreshold threshold
      (underlying.read _).expects(currentBlockHeader.id).once().returns(mempoolGraph.pure[F])
      (costMock.costOf _).expects(addedTx).returns(1000L)

      val doNotUseTxInMempoolContext = StaticTransactionValidationContext(
        currentBlockHeader.id,
        Seq.empty,
        currentBlockHeader.height,
        currentBlockHeader.slot
      )

      (semanticValidationAlgebra
        .validate(_: TransactionValidationContext)(_: IoTransaction))
        .expects(doNotUseTxInMempoolContext, addedTx)
        .once()
        .returns(
          Validated
            .invalidNec[TransactionSemanticError, IoTransaction](
              TransactionSemanticErrors.UnspendableBox(TransactionOutputAddress(0, 0, 0, addedTx.id))
            )
            .pure[F]
        )

      val res =
        for {
          mempool <- MempoolProtected.make[F](
            underlying,
            semanticValidationAlgebra,
            transactionAuthorizationVerifier,
            currentBlockHeaderF,
            fetchTransaction,
            transactionRewardCalculator,
            costMock,
            boxIdToHeight,
            config
          )

          _ <- mempool.add(addedTx.id).assertEquals(false).toResource
          _ <- mempool.add(addedTx.id).assertEquals(false).toResource

        } yield ()

      res.use_
    }
  }

  test("Do not add tx if auth check is failed") {
    withMock {
      val semanticValidationAlgebra = mock[TransactionSemanticValidationAlgebra[F]]

      val transactionAuthorizationVerifier = mock[TransactionAuthorizationVerifier[F]]

      val transactionRewardCalculator = dummyRewardCalc
      dummyCostCalc

      val boxIdToHeight = mock[TransactionOutputAddress => F[Option[Long]]]
      val maxMempoolSize = 10000
      val config =
        MempoolProtection(
          enabled = true,
          protectionEnabledThresholdPercent = 10,
          maxMempoolSize = maxMempoolSize
        )
      val currentBlockHeader = Arbitrary(ModelGenerators.headerGen()).arbitrary.first
      val currentBlockHeaderF = currentBlockHeader.pure[F]

      val underlying = mock[MempoolAlgebra[F]]
      val (addedTx, fetchTransaction) = makeTransaction
      val costMock = mock[TransactionCostCalculator]

      val txInMempool = arbitraryTx
      val txExInMempool = IoTransactionEx(txInMempool, RewardQuantities(), 500L)
      val mempoolGraph =
        MempoolGraph(Map(txInMempool.id -> txExInMempool), Map.empty, Map.empty, dummyRewardCalc, dummyCostCalc)

      (underlying.read _).expects(currentBlockHeader.id).once().returns(mempoolGraph.pure[F])
      (costMock.costOf _).expects(addedTx).returns(1000L)

      (semanticValidationAlgebra
        .validate(_: TransactionValidationContext)(_: IoTransaction))
        .expects(*, addedTx)
        .once()
        .returns(
          Validated
            .validNec[TransactionSemanticError, IoTransaction](addedTx)
            .pure[F]
        )

      (transactionAuthorizationVerifier
        .validate(_: DynamicContext[F, String, Datum])(_: IoTransaction))
        .expects(*, addedTx)
        .once()
        .returns(
          Either
            .left[TransactionAuthorizationError, IoTransaction](TransactionAuthorizationError.AuthorizationFailed())
            .pure[F]
        )

      val res =
        for {
          mempool <- MempoolProtected.make[F](
            underlying,
            semanticValidationAlgebra,
            transactionAuthorizationVerifier,
            currentBlockHeaderF,
            fetchTransaction,
            transactionRewardCalculator,
            costMock,
            boxIdToHeight,
            config
          )

          _ <- mempool.add(addedTx.id).assertEquals(false).toResource
        } yield ()

      res.use_
    }
  }

  test("Skip fee and age checks if threshold is not hit") {
    withMock {
      val transactionRewardCalculator = dummyRewardCalc
      dummyCostCalc
      val boxIdToHeight = mock[TransactionOutputAddress => F[Option[Long]]]
      val maxMempoolSize = 10000
      val config =
        MempoolProtection(
          enabled = true,
          protectionEnabledThresholdPercent = 10,
          maxMempoolSize = maxMempoolSize,
          feeFilterThresholdPercent = 100,
          ageFilterThresholdPercent = 100
        )
      val currentBlockHeader = Arbitrary(ModelGenerators.headerGen()).arbitrary.first
      val currentBlockHeaderF = currentBlockHeader.pure[F]
      val underlying = mock[MempoolAlgebra[F]]
      val (addedTx, fetchTransaction) = makeTransaction
      val (semanticValidationAlgebra, transactionAuthorizationVerifier) = semanticAndAuthCheckOk(addedTx)
      val costMock = mock[TransactionCostCalculator]

      val txInMempool = arbitraryTx
      val txExInMempool = IoTransactionEx(txInMempool, RewardQuantities(), 500L)
      val mempoolGraph =
        MempoolGraph(Map(txInMempool.id -> txExInMempool), Map.empty, Map.empty, dummyRewardCalc, dummyCostCalc)

      (underlying.read _).expects(currentBlockHeader.id).once().returns(mempoolGraph.pure[F])
      (costMock.costOf _).expects(addedTx).returns(1000L)
      (underlying.add _).expects(addedTx.id).once().returns(false.pure[F])

      val res =
        for {
          mempool <- MempoolProtected.make[F](
            underlying,
            semanticValidationAlgebra,
            transactionAuthorizationVerifier,
            currentBlockHeaderF,
            fetchTransaction,
            transactionRewardCalculator,
            costMock,
            boxIdToHeight,
            config
          )

          _ <- mempool.add(addedTx.id).assertEquals(false).toResource
        } yield ()

      res.use_
    }
  }

  test("Check fee filter") {
    PropF.forAllF(Gen.choose(0.1, 95.0)) { feeThreshold =>
      withMock {
        val boxIdToHeight = (_: TransactionOutputAddress) => 0L.some.pure[F]
        val maxMempoolSize = 10240
        val config =
          MempoolProtection(
            enabled = true,
            protectionEnabledThresholdPercent = 10,
            maxMempoolSize = maxMempoolSize,
            feeFilterThresholdPercent = feeThreshold,
            ageFilterThresholdPercent = 100,
            maxOldBoxAge = 0
          )
        val currentBlockHeader = Arbitrary(ModelGenerators.headerGen()).arbitrary.first
        val currentBlockHeaderF = currentBlockHeader.pure[F]
        val underlying = mock[MempoolAlgebra[F]]
        val (addedTx, fetchTransaction) = makeTransaction
        val (semanticValidationAlgebra, transactionAuthorizationVerifier) = semanticAndAuthCheckOk(addedTx)
        val costMock = mock[TransactionCostCalculator]
        val rewardMock = mock[TransactionRewardCalculatorAlgebra]

        val txInMempool = arbitraryTx
        val sizeInMempool = Math.round(maxMempoolSize * feeThreshold / 100)
        val feePerKbInMempool = 100.0
        val feeInMempool = feePerKbInMempool * sizeInMempool / 1024
        val freeMempoolSizePercent = (maxMempoolSize - sizeInMempool).toDouble / maxMempoolSize.toDouble
        val txExInMempool = IoTransactionEx(txInMempool, RewardQuantities(topl = feeInMempool.toLong), sizeInMempool)
        val mempoolGraph =
          MempoolGraph(Map(txInMempool.id -> txExInMempool), Map.empty, Map.empty, dummyRewardCalc, dummyCostCalc)

        (underlying.read _).expects(currentBlockHeader.id).once().returns(mempoolGraph.pure[F])
        val feeBelowTxSize = 1024
        val feeBelow = (feePerKbInMempool / freeMempoolSizePercent - feePerKbInMempool - 1) * feeBelowTxSize / 1024
        (costMock.costOf _).expects(addedTx).returns(feeBelowTxSize)
        (rewardMock.rewardsOf _).expects(addedTx).returns(RewardQuantities(topl = Math.floor(feeBelow).toLong))

        (underlying.read _).expects(currentBlockHeader.id).once().returns(mempoolGraph.pure[F])
        val feeUpperTxSize = 1024
        val feeUpper = (feePerKbInMempool / freeMempoolSizePercent - feePerKbInMempool + 1) * feeBelowTxSize / 1024
        (costMock.costOf _).expects(addedTx).returns(feeUpperTxSize)
        (rewardMock.rewardsOf _).expects(addedTx).returns(RewardQuantities(topl = Math.ceil(feeUpper).toLong))
        (underlying.add _).expects(addedTx.id).once().returns(true.pure[F])

        val res =
          for {
            mempool <- MempoolProtected.make[F](
              underlying,
              semanticValidationAlgebra,
              transactionAuthorizationVerifier,
              currentBlockHeaderF,
              fetchTransaction,
              rewardMock,
              costMock,
              boxIdToHeight,
              config
            )

            _ <- mempool.add(addedTx.id).assertEquals(false).toResource
            _ <- mempool.add(addedTx.id).assertEquals(true).toResource
          } yield ()

        res.use_
      }
    }
  }
}
