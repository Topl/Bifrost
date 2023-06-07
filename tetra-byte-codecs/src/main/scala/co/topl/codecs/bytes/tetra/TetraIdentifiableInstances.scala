package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses.ImmutableCodec
import co.topl.codecs.bytes.tetra.TetraScodecCodecs._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.crypto.hash.Blake2b256
import scala.language.implicitConversions

trait ProtoIdentifiableOps {

  implicit def blockHeaderAsBlockHeaderOps(header: BlockHeader): BlockHeaderIdOps =
    new BlockHeaderIdOps(header)
}

class BlockHeaderIdOps(val header: BlockHeader) extends AnyVal {
  import co.topl.models.utility._

  /**
   * The ID of this header.  If an ID was pre-computed and saved in the Header, it is restored.
   * Otherwise, a new ID is computed (but not saved in the Header).
   */
  def id: BlockId =
    header.headerId.getOrElse(computeId)

  /**
   * Computes what the ID _should_ be for this Header.
   */
  def computeId: BlockId =
    BlockId(
      new Blake2b256().hash(
        TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.toByteVector.toArray
      )
    )

  /**
   * Compute a new ID and return a copy of this Header with the new ID embedded.
   * Any previous value will be overwritten in the new copy.
   */
  def embedId: BlockHeader =
    header.copy(headerId = Some(computeId))

  /**
   * Returns true if this Header contains a valid embedded ID.
   */
  def containsValidId: Boolean =
    header.headerId.contains(computeId)

  /**
   * Block header immutable bytes size
   * @return
   */
  def size: Long =
    ImmutableCodec.fromScodecCodec[BlockHeader].immutableBytes(header).size
}
