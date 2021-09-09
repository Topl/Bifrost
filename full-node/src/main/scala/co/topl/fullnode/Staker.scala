package co.topl.fullnode

import cats.Id
import cats.data.OptionT
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.consensus.LeaderElection
import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.keys.{ProductPrivateKey, SymmetricKey}
import co.topl.minting.BlockMintProgram
import co.topl.minting.BlockMintProgram.UnsignedBlock
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.{Ratio, Sized}
import co.topl.typeclasses.crypto.KeyInitializer.Instances._
import co.topl.typeclasses.crypto.Proves.instances._
import co.topl.typeclasses.crypto.Signable.Instances._
import co.topl.typeclasses.crypto.{KeyInitializer, Proves}
import com.google.common.primitives.Ints

case class Staker(address: TaktikosAddress)(implicit
  clock:                   ClockAlgebra[Id],
  leaderElectionConfig:    LeaderElection.Config
) {

  private val vrfKey =
    KeyInitializer[KeyPairs.Vrf].random()

  private val initialKesKey =
    KeyInitializer[KeyPairs.Kes].random()

  private var currentKesKey: KeyPairs.Kes =
    initialKesKey

  private val scheme = new KeyEvolvingSignatureScheme

  def nextKesCertificate(unsignedBlock: UnsignedBlock, timestamp: Timestamp): Id[KesCertificate] = {
    val message =
      unsignedBlock.parentHeaderId.allBytes ++ unsignedBlock.txRoot.data ++ unsignedBlock.bloomFilter.data ++ Bytes(
        BigInt(timestamp).toByteArray
      ) ++
      Bytes(BigInt(unsignedBlock.height).toByteArray) ++
      Bytes(BigInt(unsignedBlock.slot).toByteArray) ++
      unsignedBlock.vrfCertificate.bytes ++
      Bytes(unsignedBlock.metadata.fold(Array.emptyByteArray)(_.data.value)) ++
      unsignedBlock.address.bytes

    val newSymmetricKey = scheme.updateSymmetricProductKey(
      SymmetricKey(
        ProductPrivateKey.deserializeProductKey(
          Ints.toByteArray(
            currentKesKey.privateKey.bytes.data.toArray.length
          ) ++ currentKesKey.privateKey.bytes.data.toArray
        )
      ),
      unsignedBlock.slot.toInt
    )

    currentKesKey = KeyPairs.Kes(
      PrivateKeys.Kes(
        Sized
          .strict[Bytes, PrivateKeys.Kes.Length](Bytes(ProductPrivateKey.serializer.getBytes(newSymmetricKey)))
          .toOption
          .get
      ),
      PublicKeys.Kes(
        Sized.strict[Bytes, PublicKeys.Kes.Length](Bytes(scheme.publicKey(newSymmetricKey))).toOption.get,
        unsignedBlock.slot
      )
    )

    val kesProof =
      implicitly[Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate]]
        .proveWith(currentKesKey.privateKey, message.toArray)

    val mmmProof = {
      val privKeyByteArray = currentKesKey.privateKey.bytes.data.toArray
      val sig =
        scheme.signSymmetricProduct(
          SymmetricKey(
            ProductPrivateKey.deserializeProductKey(Ints.toByteArray(privKeyByteArray.length) ++ privKeyByteArray)
          ),
          message.toArray
        )
      Proofs.Consensus.MMM(Bytes(sig.sigi), Bytes(sig.sigm), Bytes(sig.pki.bytes), sig.offset, Bytes(sig.pkl.bytes))
    }
    KesCertificate(currentKesKey.publicKey, kesProof, mmmProof)
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
      .semiflatTap(unsignedBlock => clock.delayedUntilSlot(unsignedBlock.slot))
      // Check for cancellation
      .map { unsignedBlock =>
        val timestamp = clock.currentTimestamp()
        unsignedBlock.signed(nextKesCertificate(unsignedBlock, timestamp), timestamp)
      }
  }

}
