package co.topl.http.rpc

import co.topl.akkahttprpc.Rpc
import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.box.AssetCode
import co.topl.modifier.box.AssetCode.AssetCodeVersion

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

  object Utils {

    object Seed {
      val rpc: Rpc[Params, Response] = Rpc("utils_seed")
      case class Params()
      case class Response(seed: String)
    }

    object SeedOfLength {
      val rpc: Rpc[Params, Response] = Rpc("utils_seedOfLength")
      case class Params(length: Int)
      case class Response(seed: String)
    }

    object HashBlake2b256 {
      val rpc: Rpc[Params, Response] = Rpc("utils_hashBlake2b256")
      case class Params(message: String)
      case class Response(message: String, hash: String)
    }

    object GenerateAssetCode {
      val rpc: Rpc[Params, Response] = Rpc("utils_generateAssetCode")

      case class Params(version: AssetCodeVersion, issuer: Address, shortName: String)
      case class Response(assetCode: AssetCode)
    }

    object CheckValidAddress {
      val rpc: Rpc[Params, Response] = Rpc("utils_checkValidAddress")

      case class Params(network: Option[String], address: Address)
      case class Response(address: Address, network: String)
    }
  }

}
