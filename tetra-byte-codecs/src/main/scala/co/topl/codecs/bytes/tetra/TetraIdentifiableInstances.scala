package co.topl.codecs.bytes.tetra

import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.crypto.hash.Blake2b256

import scala.language.implicitConversions

trait ProtoIdentifiableOps {

  implicit def blockHeaderAsBlockHeaderOps(header: BlockHeader): BlockHeaderIdOps =
    new BlockHeaderIdOps(header)

  implicit def ioTransactionAsIoTransactionOps(transaction: IoTransaction): IoTransactionIdOps =
    new IoTransactionIdOps(transaction)
}

class IoTransactionIdOps(val transaction: IoTransaction) extends AnyVal {

  import co.topl.brambl.common.ContainsImmutable.instances.ioTransactionImmutable
  import co.topl.brambl.common._

  def id: Identifier.IoTransaction32 =
    Identifier.IoTransaction32(ContainsEvidence[IoTransaction].sized32Evidence(transaction))

}

class BlockHeaderIdOps(val header: BlockHeader) extends AnyVal {
  import co.topl.models.utility._

  def id: BlockId =
    BlockId(
      new Blake2b256().hash(
        TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.toByteVector
      )
    )
}
