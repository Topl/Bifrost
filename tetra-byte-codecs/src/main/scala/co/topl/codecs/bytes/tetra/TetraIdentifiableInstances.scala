package co.topl.codecs.bytes.tetra

import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.TetraImmutableCodecs._
import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.{models => legacyModels}
import legacyModels._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.models.utility.ReplaceModelUtil

import scala.language.implicitConversions

trait TetraIdentifiableInstances {

  implicit val identifiableConsensusBlockHeader: Identifiable[BlockHeader] =
    (header: BlockHeader) => (IdentifierTypes.Block.HeaderV2, new Blake2b256().hash(header.immutableBytes))

  implicit val identifiableLegacyBlockHeader: Identifiable[legacyModels.BlockHeader] =
    header => identifiableConsensusBlockHeader.idOf(ReplaceModelUtil.consensusHeader(header))
}

object TetraIdentifiableInstances extends TetraIdentifiableInstances

trait ProtoIdentifiableOps {

  implicit def blockHeaderAsBlockHeaderOps(header: BlockHeader): BlockHeaderIdOps =
    new BlockHeaderIdOps(header)

  implicit def ioTransactionAsIoTransactionOps(transaction: IoTransaction): IoTransactionIdOps =
    new IoTransactionIdOps(transaction)
}

class IoTransactionIdOps(val transaction: IoTransaction) extends AnyVal {

  import co.topl.brambl.common._
  import co.topl.brambl.common.ContainsImmutable.instances.ioTransactionImmutable

  def id: Identifier.IoTransaction32 =
    Identifier.IoTransaction32(ContainsEvidence[IoTransaction].sized32Evidence(transaction))

}

class BlockHeaderIdOps(val header: BlockHeader) extends AnyVal {
  import co.topl.models.utility._

  def id: BlockId =
    BlockId(new Blake2b256().hash(header.immutableBytes))
}
