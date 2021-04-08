package co.topl.rpc

import co.topl.attestation.{Address, Proposition}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{AssetValue, Box, SimpleValue, TokenBox}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.codecs.Int128Codec
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._

trait ToplRpcServerCodecs {

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
  implicit def arbitTransferEncoder: Encoder[ArbitTransfer[Proposition]] = ArbitTransfer.jsonEncoder
  implicit def polyTransferEncoder: Encoder[PolyTransfer[Proposition]] = PolyTransfer.jsonEncoder
  implicit val tokenBoxEncoder: Encoder[TokenBox[_]] = b => Box.jsonEncoder(b)
  implicit def int128Encoder: Encoder[Int128] = Int128Codec.jsonEncoder
  implicit def int128Decoder: Decoder[Int128] = Int128Codec.jsonDecoder
//  implicit def tokenValueHolderEncoder: Encoder[TokenValueHolder] = TokenValueHolder.jsonEncoder
  implicit def simpleValueEncoder: Encoder[SimpleValue] = SimpleValue.jsonEncoder
  implicit def assetValueEncoder: Encoder[AssetValue] = AssetValue.jsonEncoder

  // Parameter Decoders

  implicit val debugDelayParamsDecoder: Decoder[ToplRpc.Debug.Delay.Params] =
    Decoder.forProduct2("blockId", "numBlocks")(ToplRpc.Debug.Delay.Params.apply)

  implicit val debugMyBlocksParamsDecoder: Decoder[ToplRpc.Debug.MyBlocks.Params] =
    deriveDecoder

  implicit val debugGeneratorsParamsDecoder: Decoder[ToplRpc.Debug.Generators.Params] =
    deriveDecoder

  implicit val debugIdsFromHeightParamsDecoder: Decoder[ToplRpc.Debug.IdsFromHeight.Params] =
    deriveDecoder

  implicit val utilsSeedParamsDecoder: Decoder[ToplRpc.Util.Seed.Params] =
    deriveDecoder

  implicit val utilsSeedOfLengthParamsDecoder: Decoder[ToplRpc.Util.SeedOfLength.Params] =
    deriveDecoder

  implicit val utilsHashBlake2b256ParamsDecoder: Decoder[ToplRpc.Util.HashBlake2b256.Params] =
    deriveDecoder

  implicit def utilsGenerateAssetCodeParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Util.GenerateAssetCode.Params] =
    Decoder.forProduct3("version", "issuer", "shortName")(ToplRpc.Util.GenerateAssetCode.Params.apply)

  implicit def utilsCheckValidAddressParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Util.CheckValidAddress.Params] =
    Decoder.forProduct2("network", "address")(ToplRpc.Util.CheckValidAddress.Params.apply)

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
    Decoder.instance(c => c.get[Transaction.TX]("tx").map(ToplRpc.Transaction.BroadcastTx.Params))

  // Response Encoders

  implicit val debugDelayResponseEncoder: Encoder[ToplRpc.Debug.Delay.Response] =
    deriveEncoder

  implicit val debugMyBlocksResponseEncoder: Encoder[ToplRpc.Debug.MyBlocks.Response] =
    Encoder.forProduct2("pubkeys", "count")(r => (r.pubkeys, r.count))

  implicit val debugGeneratorsResponseEncoder: Encoder[ToplRpc.Debug.Generators.Response] =
    r =>
      r.map { case (address, count) =>
        Address.jsonKeyEncoder(address) -> count
      }.asJson

  implicit val debugIdsFromHeightResponseEncoder: Encoder[ToplRpc.Debug.IdsFromHeight.Response] =
    _.asJson

  implicit val utilsSeedResponseEncoder: Encoder[ToplRpc.Util.Seed.Response] =
    deriveEncoder

  implicit val utilsSeedOfLengthResponseEncoder: Encoder[ToplRpc.Util.SeedOfLength.Response] =
    deriveEncoder

  implicit val utilsHashBlake2b256ResponseEncoder: Encoder[ToplRpc.Util.HashBlake2b256.Response] =
    deriveEncoder

  implicit val utilsGenerateAssetCodeResponseEncoder: Encoder[ToplRpc.Util.GenerateAssetCode.Response] =
    Encoder.forProduct1("assetCode")(_.assetCode)

  implicit val utilsCheckValidAddressResponseEncoder: Encoder[ToplRpc.Util.CheckValidAddress.Response] =
    Encoder.forProduct2("address", "network")(r => (r.address, r.network))

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

  implicit def transactionRawAssetTransferResponseEncoder(implicit
    networkPrefix: NetworkPrefix
  ): Encoder[ToplRpc.Transaction.RawAssetTransfer.Response] =
    deriveEncoder

  implicit def transactionRawArbitTransferResponseEncoder(implicit
    networkPrefix: NetworkPrefix
  ): Encoder[ToplRpc.Transaction.RawArbitTransfer.Response] =
    deriveEncoder

  implicit def transactionRawPolyTransferResponseEncoder(implicit
    networkPrefix: NetworkPrefix
  ): Encoder[ToplRpc.Transaction.RawPolyTransfer.Response] =
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
