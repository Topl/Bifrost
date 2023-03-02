package co.topl.blockchain

import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.EligibilityCertificate
import co.topl.consensus.models.OperationalCertificate
import co.topl.consensus.models.SignatureKesProduct
import co.topl.consensus.models.SignatureKesSum
import co.topl.consensus.models.VerificationKeyKesProduct
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility._
import co.topl.node.models.FullBlock
import co.topl.node.models.FullBlockBody
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import quivr.models.SmallData

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
    timestamp: Timestamp,
    outputs:   List[UnspentTransactionOutput],
    etaPrefix: Bytes = Config.DefaultEtaPrefix
  )

  object Config {

    val DefaultEtaPrefix: Bytes =
      Bytes.encodeUtf8("genesis").toOption.get

  }

  /**
   * Constructs a full block using the given Big Bang Configuration
   */
  def block(implicit config: Config): FullBlock = { // TODO move to Block.FullConsensus
    val transactions: List[Transaction] =
      List(
        IoTransaction(
          inputs = Nil,
          outputs = config.outputs,
          datum = Datum.IoTransaction(Event.IoTransaction(Schedule(Slot, Slot, 0L), SmallData.defaultInstance))
        )
      )

    val eta: Eta =
      Sized.strictUnsafe(
        new Blake2b256().hash(
          config.etaPrefix +:
          transactions.map(_.immutableBytes).toList: _*
        )
      )

    val header =
      BlockHeader(
        parentHeaderId = ParentId,
        parentSlot = ParentSlot,
        txRoot = transactions.merkleTreeRootHash.data,
        bloomFilter = transactions.bloomFilter,
        timestamp = config.timestamp,
        height = Height,
        slot = Slot,
        eligibilityCertificate = vrfCertificate(eta),
        operationalCertificate = kesCertificate,
        metadata = None,
        address = zeroBytes(Lengths.`32`)
      )
    FullBlock(header, FullBlockBody(transactions))
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
    Sized.strictUnsafe[Bytes, L](Bytes(Array.fill(l.value)(0: Byte)))
}
