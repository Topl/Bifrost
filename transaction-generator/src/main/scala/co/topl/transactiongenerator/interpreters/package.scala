package co.topl.transactiongenerator

import co.topl.brambl.common.ContainsEvidence
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models.Evidence
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.transactiongenerator.models.Wallet
import quivr.models._

package object interpreters {

  val HeightLockOneProposition: Proposition =
    Proposition(
      Proposition.Value.HeightRange(
        Proposition.HeightRange("tick", 1, Long.MaxValue)
      )
    )

  val HeightLockOneLock: Lock =
    Lock(
      Lock.Value.Predicate(
        Lock.Predicate(
          List(HeightLockOneProposition),
          1
        )
      )
    )

  val HeightLockOneEvidence: Evidence.Sized32 =
    ContainsEvidence[Lock].sized32Evidence(HeightLockOneLock)

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
      Map(HeightLockOneEvidence -> HeightLockOneLock)
    )

  /**
   * Incorporate a Transaction into a Wallet by removing spent outputs and including new outputs.
   */
  def applyTransaction(wallet: Wallet)(transaction: IoTransaction): Wallet = {
    val spentBoxIds = transaction.inputs.map(_.address)

    val transactionId = transaction.id
    val newBoxes = transaction.outputs.zipWithIndex.flatMap { case (output, index) =>
      wallet.propositions
        .get(output.address.getLock32.evidence)
        .map(lock =>
          (
            TransactionOutputAddress(0, 0, index.toShort, TransactionOutputAddress.Id.IoTransaction32(transactionId)),
            Box(lock, output.value)
          )
        )
    }
    wallet.copy(spendableBoxes = wallet.spendableBoxes -- spentBoxIds ++ newBoxes)
  }
}
