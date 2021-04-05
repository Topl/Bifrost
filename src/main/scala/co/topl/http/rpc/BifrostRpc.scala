package co.topl.http.rpc

import co.topl.akkahttprpc.Rpc
import co.topl.attestation.Address
import co.topl.modifier.ModifierId

object BifrostRpc {

  object Debug {

    object Delay {
      val rpc: Rpc[Params, Response] = Rpc("debug_delay")
      case class Params(blockId: ModifierId, numBlocks: Int)
      case class Response(delay: String)
    }

    object MyBlocks {
      val rpc: Rpc[Params, Response] = Rpc("debug_myBlocks")
      case class Params()
      case class Response(pubkeys: Set[Address], count: Int)
    }

    object Generators {
      val rpc: Rpc[Params, Response] = Rpc("debug_generators")
      case class Params()
      type Response = Map[Address, Int]
    }

    object IdsFromHeight {
      val rpc: Rpc[Params, Response] = Rpc("debug_idsFromHeight")
      case class Params(height: Long, limit: Int)
      type Response = Seq[ModifierId]
    }
  }

}
