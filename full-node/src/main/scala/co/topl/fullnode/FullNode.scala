package co.topl.fullnode

import co.topl.typeclasses.BlockGenesis

object FullNode extends App {

  val genesisBlock =
    BlockGenesis(Nil).create()

}
