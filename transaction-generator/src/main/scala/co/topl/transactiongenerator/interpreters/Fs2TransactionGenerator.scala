package co.topl.transactiongenerator.interpreters

import cats.data.Chain
import cats.effect._
import cats.effect.std.Random
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength}
import co.topl.models.utility.Sized
import co.topl.transactiongenerator.algebras.TransactionGenerator
import co.topl.transactiongenerator.models.Wallet
import co.topl.typeclasses.implicits._
import fs2._
import scala.concurrent.duration._

object Fs2TransactionGenerator {

  val HeightLockOneProposition: Proposition = Propositions.Contextual.HeightLock(1)
  val HeightLockOneSpendingAddress: SpendingAddress = HeightLockOneProposition.spendingAddress

  def make[F[_]: Async](seed: Transaction): F[TransactionGenerator[F, Stream[F, *]]] =
    for {
      wallet                       <- initialWallet(seed)
      implicit0(random: Random[F]) <- Random.javaSecuritySecureRandom
    } yield new TransactionGenerator[F, Stream[F, *]] {

      def generateTransactions: F[Stream[F, Transaction]] =
        Sync[F].delay(
          Stream
            .unfoldEval[F, Wallet, Transaction](wallet)(wallet => nextTransactionOf(wallet).map(_.some))
            .metered(100.milli)
        )
    }

  private def initialWallet[F[_]: Sync](seed: Transaction): F[Wallet] =
    Sync[F].delay(
      Wallet(
        seed.outputs.zipWithIndex
          .collect {
            case (output, index) if output.address.spendingAddress === HeightLockOneSpendingAddress =>
              val boxId = Box.Id(seed.id, index.toShort)
              val box = Box(output.address.spendingAddress.typedEvidence, output.value)
              (boxId, box)
          }
          .toIterable
          .toMap,
        Map(HeightLockOneSpendingAddress.typedEvidence -> HeightLockOneProposition)
      )
    )

  private def nextTransactionOf[F[_]: Async: Random](wallet: Wallet): F[(Transaction, Wallet)] =
    for {
      (inputBoxId, inputBox) :: _ <- Random[F].shuffleList(wallet.spendableBoxIds.toList)
      inputProposition = wallet.propositions(inputBox.evidence)
      polyBoxValue = inputBox.value.asInstanceOf[Box.Values.Poly]
      quantityOutput0 = polyBoxValue.quantity.data / 2
      output0 = Transaction.Output(
        simpleFullAddress(HeightLockOneSpendingAddress),
        Box.Values.Poly(Sized.maxUnsafe(quantityOutput0)),
        minting = false
      )
      output1 = Transaction.Output(
        simpleFullAddress(HeightLockOneSpendingAddress),
        Box.Values.Poly(Sized.maxUnsafe(polyBoxValue.quantity.data - quantityOutput0)),
        minting = false
      )
      outputs = Chain(output0, output1)
      timestamp <- Async[F].realTimeInstant
      schedule = Transaction.Schedule(timestamp.toEpochMilli, 0, Long.MaxValue)
      unprovenTransaction: Transaction.Unproven = Transaction.Unproven(
        Chain(Transaction.Unproven.Input(inputBoxId, inputProposition, inputBox.value)),
        outputs,
        schedule,
        None
      )
      transaction = unprovenTransaction.prove {
        case HeightLockOneProposition => Proofs.Contextual.HeightLock()
        case _                        => throw new MatchError()
      }
      newWallet = wallet.copy(
        spendableBoxIds = wallet.spendableBoxIds.removed(inputBoxId) ++ transaction.outputs
          .mapWithIndex((output, index) =>
            Box.Id(transaction.id, index.toShort) -> Box(output.address.spendingAddress.typedEvidence, output.value)
          )
          .toIterable,
        propositions = wallet.propositions.updated(HeightLockOneProposition.typedEvidence, HeightLockOneProposition)
      )
    } yield (transaction, newWallet)

  private[interpreters] def simpleFullAddress(spendingAddress: SpendingAddress): FullAddress =
    FullAddress(
      NetworkPrefix(0),
      spendingAddress,
      StakingAddresses.NonStaking,
      Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes.fill(64)(0: Byte)))
    )

}
