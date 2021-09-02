package co.topl.fullnode

import cats.Id
import cats.implicits._
import co.topl.algebras.Clock
import co.topl.consensus.LeaderElection
import co.topl.crypto.hash.blake2b256
import co.topl.fullnode.FullNode.taktikosAddress
import co.topl.minting.BlockMint
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.models._

case class Staker(address: TaktikosAddress)(implicit clock: Clock[Id], leaderElectionConfig: LeaderElection.Config) {

  val Right(stakerEvidence) =
    Sized.strict[TypedBytes, Lengths.`33`.type](
      TypedBytes(1: Byte, Bytes(blake2b256.hash(address.stakingVerificationKey.data.toArray).value))
    )

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

  def mintBlock(head: BlockV2, transactions: List[Transaction], relativeStake: Evidence => Ratio, epochNonce: Nonce) = {
    val interpreter = new BlockMint.Algebra[Id] {
      def address: TaktikosAddress = Staker.this.address

      def unconfirmedTransactions: Id[Seq[Transaction]] = transactions

      def elect(parent: BlockHeaderV2): BlockMint.Election = {
        val hit = LeaderElection
          .hits(
            vrfKey,
            relativeStake(stakerEvidence),
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
    unsignedBlock.signed(nextKesCertificate(unsignedBlock.slot))
  }

}
