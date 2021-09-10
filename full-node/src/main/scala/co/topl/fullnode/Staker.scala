package co.topl.fullnode

import cats.Id
import cats.data.OptionT
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.KesCertifies.instances._
import co.topl.consensus.KesCertifies.ops._
import co.topl.consensus.LeaderElection
import co.topl.minting.BlockMintProgram
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.crypto.Evolves.instances._
import co.topl.typeclasses.crypto.Evolves.ops._
import co.topl.typeclasses.crypto.KeyInitializer
import co.topl.typeclasses.crypto.KeyInitializer.Instances._

case class Staker(address: TaktikosAddress)(implicit
  clock:                   ClockAlgebra[Id],
  leaderElectionConfig:    LeaderElection.Config
) {

  private val vrfKey =
    KeyInitializer[PrivateKeys.Vrf].random()

  private var currentKesKey: PrivateKeys.Kes = {
    implicit val slot: Slot = 0L
    KeyInitializer[PrivateKeys.Kes].random()
  }

  def nextKesCertificate(unsignedBlock: BlockHeaderV2.Unsigned): Id[KesCertificate] = {
    currentKesKey = currentKesKey.evolveSteps(unsignedBlock.slot)
    currentKesKey.certify(unsignedBlock)
  }

  implicit val mint: BlockMintProgram[Id] = new BlockMintProgram[Id]

  def mintBlock(
    head:          BlockV2,
    transactions:  List[Transaction],
    relativeStake: BlockHeaderV2 => TaktikosAddress => Option[Ratio],
    epochNonce:    BlockHeaderV2 => Eta
  ): OptionT[Id, BlockV2] = {
    val interpreter = new BlockMintProgram.Algebra[Id] {
      def address: TaktikosAddress = Staker.this.address

      def unconfirmedTransactions: Id[Seq[Transaction]] = transactions

      def elect(parent: BlockHeaderV2): Option[BlockMintProgram.Election] = {
        val epoch = clockInterpreter.epochOf(parent.slot)
        relativeStake(parent)(address).flatMap(relStake =>
          LeaderElection
            .hits(
              vrfKey,
              relStake,
              fromSlot = parent.slot,
              untilSlot = clockInterpreter.epochBoundary(epoch).end,
              epochNonce(parent)
            )
            .nextOption()
            .map(hit =>
              BlockMintProgram.Election(
                slot = hit.slot,
                hit.cert,
                hit.threshold
              )
            )
        )
      }

      def canonicalHead: BlockV2 = head

      def clockInterpreter: ClockAlgebra[Id] = Staker.this.clock
    }
    mint
      .next(interpreter)
      .semiflatTap(out => clock.delayedUntilSlot(out.unsignedHeaderF(clock.currentTimestamp()).slot))
      // TODO Check for cancellation
      .map { out =>
        val timestamp = clock.currentTimestamp()
        out.signed(nextKesCertificate(out.unsignedHeaderF(timestamp)), timestamp)
      }
  }

}
