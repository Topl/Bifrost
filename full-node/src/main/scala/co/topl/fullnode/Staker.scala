package co.topl.fullnode

import cats.Id
import cats.data.OptionT
import co.topl.algebras.ClockAlgebra
import ClockAlgebra.implicits._
import co.topl.consensus.LeaderElection
import co.topl.minting.BlockMintProgram
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.typeclasses.crypto.KeyInitializer
import co.topl.typeclasses.crypto.KeyInitializer.Instances.vrfInitializer

case class Staker(address: TaktikosAddress)(implicit
  clock:                   ClockAlgebra[Id],
  leaderElectionConfig:    LeaderElection.Config
) {

  private val vrfKey =
    KeyInitializer[KeyPairs.Vrf].random()

  private val Right(initialKesKey) =
    for {
      privateKey <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
        .map(PrivateKeys.Kes(_))
      publicKey <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
        .map(PublicKeys.Kes(_, slot = 0))
    } yield Secrets.Kes(privateKey, publicKey)

  def nextKesCertificate(slot: Slot): Id[KesCertificate] = {
    val secret = initialKesKey
    KesCertificate(
      secret.publicKey,
      Proofs.Consensus.KesCertificate(
        Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(0))).toOption.get
      ),
      Proofs.Consensus.MMM(Sized.strict[Bytes, Lengths.`1440`.type](Bytes(Array.fill[Byte](1440)(0))).toOption.get),
      slotOffset = slot
    )
  }

  implicit val mint: BlockMintProgram[Id] = new BlockMintProgram[Id]

  def mintBlock(
    head:          BlockV2,
    transactions:  List[Transaction],
    relativeStake: Epoch => TaktikosAddress => Option[Ratio],
    epochNonce:    Epoch => Nonce
  ): OptionT[Id, BlockV2] = {
    val interpreter = new BlockMintProgram.Algebra[Id] {
      def address: TaktikosAddress = Staker.this.address

      def unconfirmedTransactions: Id[Seq[Transaction]] = transactions

      def elect(parent: BlockHeaderV2): Option[BlockMintProgram.Election] = {
        val epoch = clockInterpreter.epochOf(parent.slot)
        relativeStake(epoch)(address).flatMap(relStake =>
          LeaderElection
            .hits(
              vrfKey,
              relStake,
              fromSlot = parent.slot,
              untilSlot = clockInterpreter.epochBoundary(epoch).end,
              epochNonce(epoch)
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
      .semiflatTap(unsignedBlock => clock.delayedUntilSlot(unsignedBlock.slot))
      // Check for cancellation
      // Evolve KES
      .map(unsignedBlock => unsignedBlock.signed(nextKesCertificate(unsignedBlock.slot)))
  }

}
