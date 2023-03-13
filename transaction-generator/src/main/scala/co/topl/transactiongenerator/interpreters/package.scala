package co.topl.transactiongenerator

import co.topl.brambl.common.ContainsEvidence
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models._
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.transactiongenerator.models.Wallet
import quivr.models._

package object interpreters {

  val HeightLockOneProposition: Proposition =
    Proposition(
      Proposition.Value.HeightRange(
        Proposition.HeightRange("header", 1, Long.MaxValue)
      )
    )

  val HeightLockOneChallenge: Challenge =
    Challenge().withRevealed(HeightLockOneProposition)

  val HeightLockOneLock: Lock =
    Lock(
      Lock.Value.Predicate(
        Lock.Predicate(
          List(HeightLockOneChallenge),
          1
        )
      )
    )

  val HeightLockOneSpendingAddress: LockAddress =
    lockAddressOf(HeightLockOneLock)

  def lockAddressOf(lock: Lock): LockAddress =
    LockAddress(
      0,
      0,
      LockAddress.Id.Lock32(
        Identifier.Lock32(
          ContainsEvidence[Lock].sized32Evidence(lock)
        )
      )
    )

  val emptyWallet: Wallet =
    Wallet(
      Map.empty,
      Map(HeightLockOneSpendingAddress -> HeightLockOneLock)
    )

  /**
   * Incorporate a Transaction into a Wallet by removing spent outputs and including new outputs.
   */
  def applyTransaction(wallet: Wallet)(transaction: IoTransaction): Wallet = {
    val spentBoxIds = transaction.inputs.map(_.address)

    val transactionId = transaction.id
    val newBoxes = transaction.outputs.zipWithIndex.flatMap { case (output, index) =>
      wallet.propositions
        .get(output.address)
        .map(lock =>
          (
            TransactionOutputAddress(0, 0, index, TransactionOutputAddress.Id.IoTransaction32(transactionId)),
            Box(lock, output.value)
          )
        )
    }
    wallet.copy(spendableBoxes = wallet.spendableBoxes -- spentBoxIds ++ newBoxes)
  }
}
