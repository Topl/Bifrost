package co.topl.transactiongenerator

import cats.implicits._
import co.topl.models._
import co.topl.transactiongenerator.models.Wallet
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized

package object interpreters {

  val HeightLockOneProposition: Proposition = Propositions.Contextual.HeightLock(1)
  val HeightLockOneSpendingAddress: SpendingAddress = HeightLockOneProposition.spendingAddress

  val emptyWallet: Wallet =
    Wallet(
      Map.empty,
      Map(
        HeightLockOneSpendingAddress.typedEvidence ->
        HeightLockOneProposition
      )
    )

  /**
   * Incorporate a Transaction into a Wallet by removing spent outputs and including new outputs.
   */
  def applyTransaction(wallet: Wallet)(transaction: Transaction): Wallet = {
    val spentBoxIds = transaction.inputs.map(_.boxId).toIterable
    val transactionId = transaction.id.asTypedBytes
    val newBoxes = transaction.outputs.zipWithIndex.collect {
      case (output, index) if wallet.propositions.contains(output.address.spendingAddress.typedEvidence) =>
        val boxId = Box.Id(transactionId, index.toShort)
        val box = Box(output.address.spendingAddress.typedEvidence, output.value)
        (boxId, box)
    }.toIterable
    wallet.copy(spendableBoxes = wallet.spendableBoxes -- spentBoxIds ++ newBoxes)
  }

  def simpleFullAddress(spendingAddress: SpendingAddress): FullAddress =
    FullAddress(
      NetworkPrefix(0),
      spendingAddress,
      StakingAddresses.NonStaking,
      Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes.fill(64)(0: Byte)))
    )
}
