package co.topl.rpc

import cats.data.NonEmptyChain
import co.topl.akkahttprpc.Rpc
import co.topl.attestation.{Address, Proposition}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.AssetCode.AssetCodeVersion
import co.topl.modifier.box._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.modifier.transaction.Transaction.TX
import co.topl.utils.Int128

object ToplRpc {

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

  object Util {

    object Seed {
      val rpc: Rpc[Params, Response] = Rpc("util_seed")
      case class Params()
      case class Response(seed: String)
    }

    object SeedOfLength {
      val rpc: Rpc[Params, Response] = Rpc("util_seedOfLength")
      case class Params(length: Int)
      case class Response(seed: String)
    }

    object HashBlake2b256 {
      val rpc: Rpc[Params, Response] = Rpc("util_hashBlake2b256")
      case class Params(message: String)
      case class Response(message: String, hash: String)
    }

    object GenerateAssetCode {
      val rpc: Rpc[Params, Response] = Rpc("util_generateAssetCode")

      case class Params(version: AssetCodeVersion, issuer: Address, shortName: String)
      case class Response(assetCode: AssetCode)
    }

    object CheckValidAddress {
      val rpc: Rpc[Params, Response] = Rpc("util_checkValidAddress")

      case class Params(network: Option[String], address: String)
      case class Response(address: Address, network: String)
    }
  }

  object NodeView {

    object Head {
      val rpc: Rpc[Params, Response] = Rpc("topl_head")

      case class Params()
      case class Response(height: String, score: Long, bestBlockId: ModifierId, bestBlock: Block)
    }

    object Balances {
      val rpc: Rpc[Params, Response] = Rpc("topl_balances")

      case class Params(addresses: List[Address])
      type Response = Map[Address, Entry]
      case class Entry(Balances: EntryBalances, Boxes: Map[String, List[TokenBox[_]]])
      case class EntryBalances(Polys: Int128, Arbits: Int128)
    }

    object TransactionById {
      val rpc: Rpc[Params, Response] = Rpc("topl_transactionById")

      case class Params(transactionId: ModifierId)

      case class Response(
        transaction: TX,
        blockNumber: Long,
        blockId:     ModifierId
      )
    }

    object BlockById {
      val rpc: Rpc[Params, Response] = Rpc("topl_blockById")

      case class Params(blockId: ModifierId)
      type Response = Block
    }

    object BlockByHeight {
      val rpc: Rpc[Params, Response] = Rpc("topl_blockByHeight")

      case class Params(height: Long)
      type Response = Block
    }

    object Mempool {
      val rpc: Rpc[Params, Response] = Rpc("topl_mempool")

      case class Params()
      type Response = List[TX]
    }

    object TransactionFromMempool {
      val rpc: Rpc[Params, Response] = Rpc("topl_transactionFromMempool")

      case class Params(transactionId: ModifierId)

      type Response = TX
    }

    object Info {
      val rpc: Rpc[Params, Response] = Rpc("topl_info")

      case class Params()
      case class Response(network: String, nodeAddress: String, version: String)
    }
  }

  object Transaction {

    object RawAssetTransfer {
      val rpc: Rpc[Params, Response] = Rpc("topl_rawAssetTransfer")

      case class Params(
        propositionType:      String,
        sender:               NonEmptyChain[Address],
        recipients:           NonEmptyChain[(Address, AssetValue)],
        fee:                  Int128,
        changeAddress:        Address,
        consolidationAddress: Option[Address],
        minting:              Boolean,
        data:                 Option[String]
      )

      case class Response(rawTx: AssetTransfer[Proposition], messageToSign: String)
    }

    object RawArbitTransfer {
      val rpc: Rpc[Params, Response] = Rpc("topl_rawArbitTransfer")

      case class Params(
        propositionType:      String,
        sender:               NonEmptyChain[Address],
        recipients:           NonEmptyChain[(Address, Int128)],
        fee:                  Int128,
        changeAddress:        Address,
        consolidationAddress: Option[Address],
        data:                 Option[String]
      )

      case class Response(rawTx: ArbitTransfer[Proposition], messageToSign: String)
    }

    object RawPolyTransfer {
      val rpc: Rpc[Params, Response] = Rpc("topl_rawPolyTransfer")

      case class Params(
        propositionType: String,
        sender:          NonEmptyChain[Address],
        recipients:      NonEmptyChain[(Address, Int128)],
        fee:             Int128,
        changeAddress:   Address,
        data:            Option[String]
      )

      case class Response(rawTx: PolyTransfer[Proposition], messageToSign: String)
    }

    object BroadcastTx {
      val rpc: Rpc[Params, Response] = Rpc("topl_broadcastTx")

      case class Params(tx: TX)

      type Response = TX
    }
  }

}
