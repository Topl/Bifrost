package co.topl.blockchain

import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.consensus.models._
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString

/**
 * The beginning of everything.  ("everything" of course just means the first block of a blockchain)
 */
object BigBang {

  /**
   * Represents a way of configuring the variables of the Big Bang Block.
   * @param timestamp The unix epoch (in milliseconds) of the block.  Also represents the timestamp of the first slot
   *                  of the blockchain's clock.
   * @param outputs The initial set of outputs for the blockchain.  This generally includes the initial distribution
   *                to the blockchain's initial investors.  In addition, it should specify the staking registrations
   *                for the blockchain's initial operators.
   * @param etaPrefix a sequence of bytes to be prepended to the value that gets hashed to produce the Big Bang Eta.
   */
  case class Config(
    timestamp:    Timestamp,
    transactions: List[IoTransaction],
    etaPrefix:    Bytes = Config.DefaultEtaPrefix
  )

  object Config {

    val DefaultEtaPrefix: Bytes = ByteString.copyFromUtf8("genesis")

  }

  /**
   * Constructs a full block using the given Big Bang Configuration
   */
  def block(implicit config: Config): FullBlock = {
    val eta: Eta =
      Sized.strictUnsafe(
        new Blake2b256().hash(
          (config.etaPrefix +:
          config.transactions.map(_.id.value)).map(v => v: Array[Byte]): _*
        )
      )

    val header =
      BlockHeader(
        parentHeaderId = ParentId,
        parentSlot = ParentSlot,
        txRoot = config.transactions.merkleTreeRootHash.data,
        bloomFilter = config.transactions.bloomFilter.data,
        timestamp = config.timestamp,
        height = Height,
        slot = Slot,
        eligibilityCertificate = vrfCertificate(eta),
        operationalCertificate = kesCertificate,
        metadata = ByteString.EMPTY,
        address = StakingAddress(zeroBytes(Lengths.`32`).data)
      )
    FullBlock(header, FullBlockBody(config.transactions))
  }

  val ParentId: BlockId = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(0)))
  val ParentSlot: Slot = -1
  val Slot = 0
  val Height = 1

  def vrfCertificate(eta: Eta): EligibilityCertificate = EligibilityCertificate(
    ByteString.copyFrom(Array.fill[Byte](80)(0)),
    ByteString.copyFrom(Array.fill[Byte](32)(0)),
    ByteString.copyFrom(Array.fill[Byte](32)(0)),
    eta = eta.data
  )

  val kesCertificate: OperationalCertificate = OperationalCertificate(
    VerificationKeyKesProduct(ByteString.copyFrom(Array.fill[Byte](32)(0)), 0),
    SignatureKesProduct(
      SignatureKesSum(
        ByteString.copyFrom(Array.fill[Byte](32)(0)),
        ByteString.copyFrom(Array.fill[Byte](64)(0)),
        Vector.empty
      ),
      SignatureKesSum(
        ByteString.copyFrom(Array.fill[Byte](32)(0)),
        ByteString.copyFrom(Array.fill[Byte](64)(0)),
        Vector.empty
      ),
      ByteString.copyFrom(Array.fill[Byte](32)(0))
    ),
    ByteString.copyFrom(Array.fill[Byte](32)(0)),
    ByteString.copyFrom(Array.fill[Byte](64)(0))
  )

  def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe[Bytes, L](ByteString.copyFrom(Array.fill(l.value)(0: Byte)))
}
