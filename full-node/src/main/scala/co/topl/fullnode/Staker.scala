package co.topl.fullnode

import cats.Id
import co.topl.algebras.Clock
import co.topl.consensus.LeaderElection
import co.topl.minting.BlockMint
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.{Lengths, Ratio, Sized}

case class Staker(address: TaktikosAddress)(implicit clock: Clock[Id], leaderElectionConfig: LeaderElection.Config) {

  private val Right(vrfKey) =
    for {
      privateKey <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
        .map(PrivateKeys.Ed25519(_))
        .map(PrivateKeys.Vrf)
      publicKey <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
        .map(PublicKeys.Ed25519(_))
        .map(PublicKeys.Vrf)
    } yield KeyPairs.Vrf(privateKey, publicKey)

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
    relativeStake: TaktikosAddress => Ratio,
    epochNonce:    Nonce
  ): BlockV2 = {
    val interpreter = new BlockMint.Algebra[Id] {
      def address: TaktikosAddress = Staker.this.address

      def unconfirmedTransactions: Id[Seq[Transaction]] = transactions

      def elect(parent: BlockHeaderV2): BlockMint.Election = {
        val hit = LeaderElection
          .hits(
            vrfKey,
            relativeStake(address),
            fromSlot = parent.slot,
            epochNonce
          )
          .head

        BlockMint.Election(
          slot = hit.slot,
          hit.cert,
          hit.threshold
        )
      }

      def currentHead: BlockV2 = head

      def clock: Clock[Id] = Staker.this.clock
    }

    val unsignedBlock =
      mint.next(interpreter)
    clock.delayedUntilSlot(unsignedBlock.slot)
    // Check for cancellation
    // Evolve KES
    unsignedBlock.signed(nextKesCertificate(unsignedBlock.slot))
  }

}
