package co.topl.rpc

import co.topl.attestation.{Address, Proposition}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{AssetValue, Box, SimpleValue, TokenBox}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.codecs.Int128Codec
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.{Decoder, Encoder}

trait ToplRpcCodecs extends ToplRpcClientCodecs with ToplRpcServerCodecs

trait ToplRpcClientCodecs
    extends DebugRpcParamsEncoders
    with UtilRpcParamsEncoders
    with NodeViewRpcParamsEncoders
    with TransactionRpcParamsEncoders
    with DebugRpcResponseDecoders
    with UtilRpcResponseDecoders
    with NodeViewRpcResponseDecoders
    with TransactionRpcResponseDecoders

trait DebugRpcParamsEncoders {

  implicit val debugDelayParamsEncoder: Encoder[ToplRpc.Debug.Delay.Params] =
    deriveEncoder

  implicit val debugMyBlocksParamsEncoder: Encoder[ToplRpc.Debug.MyBlocks.Params] =
    deriveEncoder

  implicit val debugGeneratorsParamsEncoder: Encoder[ToplRpc.Debug.Generators.Params] =
    deriveEncoder

  implicit val debugIdsFromHeightParamsEncoder: Encoder[ToplRpc.Debug.IdsFromHeight.Params] =
    deriveEncoder

}

trait UtilRpcParamsEncoders {

  implicit val utilsSeedParamsEncoder: Encoder[ToplRpc.Util.Seed.Params] =
    deriveEncoder

  implicit val utilsSeedOfLengthParamsEncoder: Encoder[ToplRpc.Util.SeedOfLength.Params] =
    deriveEncoder

  implicit val utilsHashBlake2b256ParamsEncoder: Encoder[ToplRpc.Util.HashBlake2b256.Params] =
    deriveEncoder

  implicit val utilsGenerateAssetCodeParamsEncoder: Encoder[ToplRpc.Util.GenerateAssetCode.Params] =
    deriveEncoder

  implicit val utilsCheckValidAddressParamsEncoder: Encoder[ToplRpc.Util.CheckValidAddress.Params] =
    deriveEncoder
}

trait NodeViewRpcParamsEncoders {

  implicit val nodeViewHeadParamsEncoder: Encoder[ToplRpc.NodeView.Head.Params] =
    deriveEncoder

  implicit val nodeViewBalancesParamsEncoder: Encoder[ToplRpc.NodeView.Balances.Params] =
    deriveEncoder

  implicit val nodeViewTransactionByIdParamsEncoder: Encoder[ToplRpc.NodeView.TransactionById.Params] =
    deriveEncoder

  implicit val nodeViewBlockByIdParamsEncoder: Encoder[ToplRpc.NodeView.BlockById.Params] =
    deriveEncoder

  implicit val nodeViewBlockByHeightParamsEncoder: Encoder[ToplRpc.NodeView.BlockByHeight.Params] =
    deriveEncoder

  implicit val nodeViewMempoolParamsEncoder: Encoder[ToplRpc.NodeView.Mempool.Params] =
    deriveEncoder

  implicit val nodeViewTransactionFromMempoolParamsEncoder: Encoder[ToplRpc.NodeView.TransactionFromMempool.Params] =
    deriveEncoder

  implicit val nodeViewInfoParamsEncoder: Encoder[ToplRpc.NodeView.Info.Params] =
    deriveEncoder
}

trait TransactionRpcParamsEncoders extends SharedCodecs {

  implicit val transactionRawAssetTransferParamsEncoder: Encoder[ToplRpc.Transaction.RawAssetTransfer.Params] =
    deriveEncoder

  implicit val transactionRawArbitTransferParamsEncoder: Encoder[ToplRpc.Transaction.RawArbitTransfer.Params] =
    deriveEncoder

  implicit val transactionRawPolyTransferParamsEncoder: Encoder[ToplRpc.Transaction.RawPolyTransfer.Params] =
    deriveEncoder

//  implicit val transactionBroadcastParamsEncoder: Encoder[ToplRpc.Transaction.BroadcastTx.Params] =
//    deriveEncoder

}

trait DebugRpcResponseDecoders {

  implicit val debugDelayResponseDecoder: Decoder[ToplRpc.Debug.Delay.Response] =
    deriveDecoder

  implicit def debugMyBlocksResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Debug.MyBlocks.Response] =
    deriveDecoder

  implicit val debugIdsFromHeightResponseDecoder: Decoder[ToplRpc.Debug.IdsFromHeight.Response] =
    Decoder.decodeList
}

trait UtilRpcResponseDecoders {

  implicit val utilsSeedResponseDecoder: Decoder[ToplRpc.Util.Seed.Response] =
    deriveDecoder

  implicit val utilsSeedOfLengthResponseDecoder: Decoder[ToplRpc.Util.SeedOfLength.Response] =
    deriveDecoder

  implicit val utilsHashBlake2b256ResponseDecoder: Decoder[ToplRpc.Util.HashBlake2b256.Response] =
    deriveDecoder

  implicit val utilsGenerateAssetCodeResponseDecoder: Decoder[ToplRpc.Util.GenerateAssetCode.Response] =
    deriveDecoder

  implicit def utilsCheckValidAddressResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Util.CheckValidAddress.Response] =
    deriveDecoder
}

trait NodeViewRpcResponseDecoders extends SharedCodecs {

  implicit def nodeViewHeadResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.Head.Response] =
    deriveDecoder

//  implicit val nodeViewBalancesResponseDecoder: Decoder[ToplRpc.NodeView.Balances.Response] =
//    deriveDecoder

  implicit def nodeViewTransactionByIdResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.TransactionById.Response] =
    Decoder.forProduct3("transaction", "blockNumber", "blockId")(ToplRpc.NodeView.TransactionById.Response.apply)

//  implicit val nodeViewBlockByIdResponseDecoder: Decoder[ToplRpc.NodeView.BlockById.Response] =
//    deriveDecoder

//  implicit val nodeViewBlockByHeightResponseDecoder: Decoder[ToplRpc.NodeView.BlockByHeight.Response] =
//    deriveDecoder

//  implicit val nodeViewMempoolResponseDecoder: Decoder[ToplRpc.NodeView.Mempool.Response] =
//    deriveDecoder

//  implicit val nodeViewTransactionFromMempoolResponseDecoder
//    : Decoder[ToplRpc.NodeView.TransactionFromMempool.Response] =
//    deriveDecoder

  implicit val nodeViewInfoResponseDecoder: Decoder[ToplRpc.NodeView.Info.Response] =
    deriveDecoder
}

trait TransactionRpcResponseDecoders extends SharedCodecs {

  implicit def transactionRawAssetTransferResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawAssetTransfer.Response] =
    deriveDecoder

  implicit def transactionRawArbitTransferResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawArbitTransfer.Response] =
    deriveDecoder

  implicit def transactionRawPolyTransferResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawPolyTransfer.Response] =
    deriveDecoder
//  implicit def transactionBroadcastResponseDecoder(implicit networkPrefix: NetworkPrefix): Decoder[ToplRpc.Transaction.BroadcastTx.Response] =
//    deriveDecoder
}

trait ToplRpcServerCodecs
    extends DebugRpcParamsDecoders
    with UtilRpcParamsDecoders
    with NodeViewRpcParamsDecoders
    with TransactionRpcParamsDecoders
    with DebugRpcResponseEncoders
    with UtilRpcResponseEncoders
    with NodeViewRpcResponseEncoders
    with TransactionRpcResponseEncoders

trait DebugRpcParamsDecoders extends SharedCodecs {

  implicit val debugDelayParamsDecoder: Decoder[ToplRpc.Debug.Delay.Params] =
    deriveDecoder

  implicit val debugMyBlocksParamsDecoder: Decoder[ToplRpc.Debug.MyBlocks.Params] =
    deriveDecoder

  implicit val debugGeneratorsParamsDecoder: Decoder[ToplRpc.Debug.Generators.Params] =
    deriveDecoder

  implicit val debugIdsFromHeightParamsDecoder: Decoder[ToplRpc.Debug.IdsFromHeight.Params] =
    deriveDecoder
}

trait UtilRpcParamsDecoders extends SharedCodecs {

  implicit val utilsSeedParamsDecoder: Decoder[ToplRpc.Util.Seed.Params] =
    deriveDecoder

  implicit val utilsSeedOfLengthParamsDecoder: Decoder[ToplRpc.Util.SeedOfLength.Params] =
    deriveDecoder

  implicit val utilsHashBlake2b256ParamsDecoder: Decoder[ToplRpc.Util.HashBlake2b256.Params] =
    deriveDecoder

  implicit def utilsGenerateAssetCodeParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Util.GenerateAssetCode.Params] =
    deriveDecoder

  implicit def utilsCheckValidAddressParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Util.CheckValidAddress.Params] =
    deriveDecoder
}

trait NodeViewRpcParamsDecoders {

  implicit val nodeViewHeadParamsDecoder: Decoder[ToplRpc.NodeView.Head.Params] =
    deriveDecoder

  implicit def nodeViewBalancesParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.Balances.Params] =
    deriveDecoder

  implicit val nodeViewTransactionsByIdParamsDecoder: Decoder[ToplRpc.NodeView.TransactionById.Params] =
    deriveDecoder

  implicit val nodeViewBlocksByIdParamsDecoder: Decoder[ToplRpc.NodeView.BlockById.Params] =
    deriveDecoder

  implicit val nodeViewBlocksByHeightParamsDecoder: Decoder[ToplRpc.NodeView.BlockByHeight.Params] =
    deriveDecoder

  implicit val nodeViewMempoolParamsDecoder: Decoder[ToplRpc.NodeView.Mempool.Params] =
    deriveDecoder

  implicit val nodeViewTransactionFromMempoolParamsDecoder: Decoder[ToplRpc.NodeView.TransactionFromMempool.Params] =
    deriveDecoder

  implicit val nodeViewInfoParamsDecoder: Decoder[ToplRpc.NodeView.Info.Params] =
    deriveDecoder
}

trait TransactionRpcParamsDecoders extends SharedCodecs {

  implicit def transactionRawAssetTransferParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawAssetTransfer.Params] =
    deriveDecoder

  implicit def transactionRawArbitTransferParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawArbitTransfer.Params] =
    deriveDecoder

  implicit def transactionRawPolyTransferParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawPolyTransfer.Params] =
    deriveDecoder

  implicit def transactionBroadcastTxParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.BroadcastTx.Params] =
    Decoder.forProduct1("tx")(ToplRpc.Transaction.BroadcastTx.Params.apply)

}

trait DebugRpcResponseEncoders extends SharedCodecs {

  implicit val debugDelayResponseEncoder: Encoder[ToplRpc.Debug.Delay.Response] =
    deriveEncoder

  implicit val debugMyBlocksResponseEncoder: Encoder[ToplRpc.Debug.MyBlocks.Response] =
    deriveEncoder

  implicit val debugGeneratorsResponseEncoder: Encoder[ToplRpc.Debug.Generators.Response] =
    r =>
      r.map { case (address, count) =>
        Address.jsonKeyEncoder(address) -> count
      }.asJson

  implicit val debugIdsFromHeightResponseEncoder: Encoder[ToplRpc.Debug.IdsFromHeight.Response] =
    Encoder.encodeList
}

trait UtilRpcResponseEncoders extends SharedCodecs {

  implicit val utilsSeedResponseEncoder: Encoder[ToplRpc.Util.Seed.Response] =
    deriveEncoder

  implicit val utilsSeedOfLengthResponseEncoder: Encoder[ToplRpc.Util.SeedOfLength.Response] =
    deriveEncoder

  implicit val utilsHashBlake2b256ResponseEncoder: Encoder[ToplRpc.Util.HashBlake2b256.Response] =
    deriveEncoder

  implicit val utilsGenerateAssetCodeResponseEncoder: Encoder[ToplRpc.Util.GenerateAssetCode.Response] =
    deriveEncoder

  implicit val utilsCheckValidAddressResponseEncoder: Encoder[ToplRpc.Util.CheckValidAddress.Response] =
    deriveEncoder
}

trait NodeViewRpcResponseEncoders extends SharedCodecs {

  implicit val nodeViewHeadResponseEncoder: Encoder[ToplRpc.NodeView.Head.Response] =
    deriveEncoder

  implicit val nodeViewBalancesResponseEntryEncoder: Encoder[ToplRpc.NodeView.Balances.Entry] =
    deriveEncoder

  implicit val nodeViewBalancesResponseEntryBalancesEncoder: Encoder[ToplRpc.NodeView.Balances.EntryBalances] =
    deriveEncoder

  implicit val nodeViewBalancesResponseEncoder: Encoder[ToplRpc.NodeView.Balances.Response] =
    _.map { case (address, entry) =>
      Address.jsonKeyEncoder(address) -> entry
    }.asJson

  implicit val nodeViewTransactionByIdResponseEncoder: Encoder[ToplRpc.NodeView.TransactionById.Response] =
    r =>
      Map(
        "blockNumber" -> r.blockNumber.asJson,
        "blockId"     -> r.blockId.asJson
      ).asJson.deepMerge(r.transaction.asJson)

  implicit val nodeViewInfoResponseEncoder: Encoder[ToplRpc.NodeView.Info.Response] =
    deriveEncoder
}

trait TransactionRpcResponseEncoders extends SharedCodecs {

  implicit val transactionRawAssetTransferResponseEncoder: Encoder[ToplRpc.Transaction.RawAssetTransfer.Response] =
    deriveEncoder

  implicit val transactionRawArbitTransferResponseEncoder: Encoder[ToplRpc.Transaction.RawArbitTransfer.Response] =
    deriveEncoder

  implicit val transactionRawPolyTransferResponseEncoder: Encoder[ToplRpc.Transaction.RawPolyTransfer.Response] =
    deriveEncoder

  // These are here but commented out because they can already be derived from previous implicits
  //  implicit val nodeViewMempoolResponseEncoder: Encoder[ToplRpc.NodeView.Mempool.Response] =
  //    deriveEncoder
  //  implicit val nodeViewTransactionFromMempoolResponseEncoder: Encoder[ToplRpc.NodeView.TransactionFromMempool.Response] =
  //    deriveEncoder
  //  implicit val nodeViewBlockByIdResponseEncoder: Encoder[ToplRpc.NodeView.BlockById.Response] =
  //    deriveEncoder
  //  implicit val nodeViewBlockByHeightResponseEncoder: Encoder[ToplRpc.NodeView.BlockByHeight.Response] =
  //    deriveEncoder
  //  implicit val transactionBroadcastTxResponseEncoder: Encoder[ToplRpc.Transaction.BroadcastTx.Response] =
  //    deriveEncoder
}

trait SharedCodecs {

  implicit def blockEncoder: Encoder[Block] = Block.jsonEncoder
  implicit def blockDecoder(implicit networkPrefix: NetworkPrefix): Decoder[Block] = Block.jsonDecoder
  implicit def modifierIdEncoder: Encoder[ModifierId] = ModifierId.jsonEncoder
  implicit def modifierIdDecoder: Decoder[ModifierId] = ModifierId.jsonDecoder
  implicit def addressEncoder: Encoder[Address] = Address.jsonEncoder
  implicit def addressDecoder(implicit networkPrefix: NetworkPrefix): Decoder[Address] = Address.jsonDecoder
  implicit def transactionEncoder: Encoder[Transaction.TX] = Transaction.jsonEncoder

  implicit def transactionDecoder(implicit networkPrefix: NetworkPrefix): Decoder[Transaction.TX] =
    Transaction.jsonDecoder
  implicit def assetTransferEncoder: Encoder[AssetTransfer[Proposition]] = AssetTransfer.jsonEncoder

  implicit def assetTransferDecoder(implicit networkPrefix: NetworkPrefix): Decoder[AssetTransfer[Proposition]] =
    Decoder
      .instance(c => AssetTransfer.jsonDecoder.apply(c))
      .map { case a: AssetTransfer[Proposition @unchecked] =>
        a
      }
  implicit def arbitTransferEncoder: Encoder[ArbitTransfer[Proposition]] = ArbitTransfer.jsonEncoder

  implicit def arbitTransferDecoder(implicit networkPrefix: NetworkPrefix): Decoder[ArbitTransfer[Proposition]] =
    Decoder
      .instance(c => ArbitTransfer.jsonDecoder.apply(c))
      .map { case a: ArbitTransfer[Proposition @unchecked] =>
        a
      }
  implicit def polyTransferEncoder: Encoder[PolyTransfer[Proposition]] = PolyTransfer.jsonEncoder

  implicit def polyTransferDecoder(implicit networkPrefix: NetworkPrefix): Decoder[PolyTransfer[Proposition]] =
    Decoder
      .instance(c => PolyTransfer.jsonDecoder.apply(c))
      .map { case a: PolyTransfer[Proposition @unchecked] =>
        a
      }
  implicit val tokenBoxEncoder: Encoder[TokenBox[_]] = b => Box.jsonEncoder(b)
  implicit def int128Encoder: Encoder[Int128] = Int128Codec.jsonEncoder
  implicit def int128Decoder: Decoder[Int128] = Int128Codec.jsonDecoder
  implicit def simpleValueEncoder: Encoder[SimpleValue] = SimpleValue.jsonEncoder
  implicit def assetValueEncoder: Encoder[AssetValue] = AssetValue.jsonEncoder
}
