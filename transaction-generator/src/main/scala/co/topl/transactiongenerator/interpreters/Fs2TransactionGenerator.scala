package co.topl.transactiongenerator.interpreters

import cats.data.Chain
import cats.effect._
import cats.effect.std.Random
import cats.implicits._
import cats.Parallel
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.{bigIntLength, bytesLength, latin1DataLength}
import co.topl.models.utility.Sized
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.transactiongenerator.algebras.TransactionGenerator
import co.topl.transactiongenerator.models.Wallet
import co.topl.typeclasses.implicits._
import fs2._

object Fs2TransactionGenerator {

  val HeightLockOneProposition: Proposition = Propositions.Contextual.HeightLock(1)
  val HeightLockOneSpendingAddress: SpendingAddress = HeightLockOneProposition.spendingAddress

  /**
   * Interprets TransactionGenerator using a given `Wallet` and FS2.  Emits a never-ending stream of Transactions,
   * updating the local wallet along the way.
   * @param wallet An initial wallet containing an initial set of spendable UTxOs
   */
  def make[F[_]: Async: Parallel](wallet: Wallet): F[TransactionGenerator[F, Stream[F, *]]] =
    for {
      implicit0(random: Random[F]) <- Random.javaSecuritySecureRandom
    } yield new TransactionGenerator[F, Stream[F, *]] {

      def generateTransactions: F[Stream[F, Transaction]] =
        Sync[F].delay(
          Stream
            .unfoldEval[F, Vector[Wallet], Vector[Transaction]](Vector(wallet))(wallets =>
              // In the current implementation, wallets grow in size and never shrink.  Once a wallet grows too large,
              // split it into two separate wallets and run them in parallel
              wallets
                .flatMap(wallet =>
                  if (wallet.spendableBoxIds.size > 10) WalletSplitter.split(wallet, 2)
                  else Vector(wallet)
                )
                .parTraverse(nextTransactionOf[F])
                .map(results => (results.map(_._1), results.map(_._2)).some)
            )
            .flatMap(Stream.iterable)
        )
    }

  /**
   * Given a _current_ wallet, produce a new Transaction and new Wallet.  The generated transaction
   * will spend a random input from the wallet and produce two new outputs
   */
  private def nextTransactionOf[F[_]: Async: Random](wallet: Wallet): F[(Transaction, Wallet)] =
    for {
      (inputBoxId, inputBox) <- wallet.spendableBoxIds.toList
        .maxBy(_._2.value.asInstanceOf[Box.Values.Poly].quantity.data)
        .pure[F]
      inputProposition = wallet.propositions(inputBox.evidence)
      polyBoxValue = inputBox.value.asInstanceOf[Box.Values.Poly]
      outputs =
        if (polyBoxValue.quantity.data > 1) {
          val quantityOutput0 = polyBoxValue.quantity.data / 2
          val output0 = Transaction.Output(
            simpleFullAddress(HeightLockOneSpendingAddress),
            Box.Values.Poly(Sized.maxUnsafe(quantityOutput0)),
            minting = false
          )
          val output1 = Transaction.Output(
            simpleFullAddress(HeightLockOneSpendingAddress),
            Box.Values.Poly(Sized.maxUnsafe(polyBoxValue.quantity.data - quantityOutput0)),
            minting = false
          )
          Chain(output0, output1)
        } else {
          Chain(
            Transaction.Output(
              simpleFullAddress(HeightLockOneSpendingAddress),
              polyBoxValue,
              minting = false
            )
          )
        }
      timestamp <- Async[F].realTimeInstant
      schedule = Transaction.Schedule(timestamp.toEpochMilli, 0, Long.MaxValue)
      dataBytes <- Array.fill[Byte](100)(3).pure[F]
      unprovenTransaction: Transaction.Unproven = Transaction.Unproven(
        Chain(Transaction.Unproven.Input(inputBoxId, inputProposition, inputBox.value)),
        outputs,
        schedule,
        Some(Sized.maxUnsafe(Latin1Data.fromData(dataBytes)))
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
