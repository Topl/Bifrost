package co.topl

import cats.implicits.toShow
import co.topl.consensus.models.BlockId
import co.topl.typeclasses.implicits.showBlockId

package object node {

  /**
   * For private testnets, it is convenient to run each network from its own directory, rather than re-using
   * a single directory over and over.  This method helps interpolate a directory
   * (i.e. /tmp/bifrost/data/{genesisBlockId}) with the genesis block ID.
   */
  def interpolateBlockId(genesisBlockId: BlockId)(path: String): String =
    path.replace("{genesisBlockId}", genesisBlockId.show)
}
