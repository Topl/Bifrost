package co.topl.fullnode

import cats.Id
import cats.data.OptionT
import co.topl.algebras.Clock
import Clock.implicits._
import co.topl.consensus.LeaderElection
import co.topl.minting.BlockMint
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.typeclasses.crypto.KeyInitializer
import co.topl.typeclasses.crypto.KeyInitializer.Instances.vrfInitializer

case class Staker(address: TaktikosAddress)(implicit clock: Clock[Id], leaderElectionConfig: LeaderElection.Config) {

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

  implicit val mint: BlockMint[Id] = new BlockMint[Id]

  def mintBlock(
    head:          BlockV2,
    transactions:  List[Transaction],
    relativeStake: Epoch => TaktikosAddress => Option[Ratio],
    epochNonce:    Epoch => Nonce
  ): OptionT[Id, BlockV2] = {
    val interpreter = new BlockMint.Algebra[Id] {
      def address: TaktikosAddress = Staker.this.address

      def unconfirmedTransactions: Id[Seq[Transaction]] = transactions

      def elect(parent: BlockHeaderV2): Option[BlockMint.Election] = {
        val epoch = clock.epochOf(parent.slot)
        relativeStake(epoch)(address).flatMap(relStake =>
          LeaderElection
            .hits(
              vrfKey,
              relStake,
              fromSlot = parent.slot,
              untilSlot = clock.epochBoundary(epoch).end,
              epochNonce(epoch)
            )
            .nextOption()
            .map(hit =>
              BlockMint.Election(
                slot = hit.slot,
                hit.cert,
                hit.threshold
              )
            )
        )
      }

      def canonicalHead: BlockV2 = head

      def clock: Clock[Id] = Staker.this.clock
    }
    mint
      .next(interpreter)
      .semiflatTap(unsignedBlock => clock.delayedUntilSlot(unsignedBlock.slot))
      // Check for cancellation
      // Evolve KES
      .map(unsignedBlock => unsignedBlock.signed(nextKesCertificate(unsignedBlock.slot)))
  }

}
