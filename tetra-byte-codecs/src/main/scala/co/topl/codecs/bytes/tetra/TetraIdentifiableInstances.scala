package co.topl.codecs.bytes.tetra

import co.topl.brambl.common.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.models.common.ImmutableBytes
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.crypto.hash.Blake2b256
import co.topl.brambl.common._
import co.topl.brambl.models.TransactionId

import scala.language.implicitConversions

trait ProtoIdentifiableOps {

  implicit def blockHeaderAsBlockHeaderOps(header: BlockHeader): BlockHeaderIdOps =
    new BlockHeaderIdOps(header)

  implicit def ioTransactionAsIoTransactionOps(transaction: IoTransaction): IoTransactionIdOps =
    new IoTransactionIdOps(transaction)
}

class IoTransactionIdOps(val transaction: IoTransaction) extends AnyVal {

  def id: TransactionId = {
    import IoTransactionIdOps._
    val signableBytes = ContainsSignable[IoTransaction].signableBytes(transaction)
    val immutable = ImmutableBytes(signableBytes.value)
    val evidence = ContainsEvidence[ImmutableBytes].sizedEvidence(immutable)
    TransactionId(evidence.digest.value)
  }

}

object IoTransactionIdOps {
  implicit private val immutableContainsImmutable: ContainsImmutable[ImmutableBytes] = identity
}

class BlockHeaderIdOps(val header: BlockHeader) extends AnyVal {
  import co.topl.models.utility._

  def id: BlockId =
    BlockId(
      new Blake2b256().hash(
        TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.toByteVector.toArray
      )
    )
}
