package co.topl.rpc

import cats.data.NonEmptyChain
import co.topl.attestation.{Address, Proposition}
import co.topl.codecs.json.JsonCodecs
import co.topl.codecs.json.tetra.ModelsJsonCodecs
import co.topl.modifier.ModifierId
import co.topl.modifier.box._
import co.topl.modifier.transaction.builder.{BoxSelectionAlgorithm, BoxSelectionAlgorithms}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._

trait ToplRpcCodecs extends ToplRpcClientCodecs with ToplRpcServerCodecs

trait ToplRpcClientCodecs
    extends DebugRpcParamsEncoders
    with UtilRpcParamsEncoders
    with NodeViewRpcParamsEncoders
    with TransactionRpcParamsEncoders
    with AdminRpcParamsEncoders
    with DebugRpcResponseDecoders
    with UtilRpcResponseDecoders
    with NodeViewRpcResponseDecoders
    with TransactionRpcResponseDecoders
    with AdminRpcResponseDecoders

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

  implicit val nodeViewBlocksByIdsParamsEncoder: Encoder[ToplRpc.NodeView.BlocksByIds.Params] =
    deriveEncoder

  implicit val nodeViewBlockByHeightParamsEncoder: Encoder[ToplRpc.NodeView.BlockByHeight.Params] =
    deriveEncoder

  implicit val nodeViewBlocksInRangeParamsEncoder: Encoder[ToplRpc.NodeView.BlocksInRange.Params] =
    deriveEncoder

  implicit val nodeViewBlockIdsInRangeParamsEncoder: Encoder[ToplRpc.NodeView.BlockIdsInRange.Params] =
    deriveEncoder

  implicit val nodeViewLatestBlocksParamsEncoder: Encoder[ToplRpc.NodeView.LatestBlocks.Params] =
    deriveEncoder

  implicit val nodeViewLatestBlockIdsParamsEncoder: Encoder[ToplRpc.NodeView.LatestBlockIds.Params] =
    deriveEncoder

  implicit val nodeViewMempoolParamsEncoder: Encoder[ToplRpc.NodeView.Mempool.Params] =
    deriveEncoder

  implicit val nodeViewTransactionFromMempoolParamsEncoder: Encoder[ToplRpc.NodeView.TransactionFromMempool.Params] =
    deriveEncoder

  implicit val nodeViewConfirmationStatusParamsEncoder: Encoder[ToplRpc.NodeView.ConfirmationStatus.Params] =
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

  implicit val transactionBroadcastTxParamsEncoder: Encoder[ToplRpc.Transaction.BroadcastTx.Params] =
    Encoder.forProduct1("tx")(_.tx)

  implicit val transactionEncodeTransferParamsEncoder: Encoder[ToplRpc.Transaction.EncodeTransfer.Params] =
    Encoder.forProduct1("unprovenTransaction")(_.unprovenTransaction)

  implicit val unprovenPolyTransferParamsEncoder: Encoder[ToplRpc.Transaction.UnprovenPolyTransfer.Params] =
    deriveEncoder

  implicit val transactionBroadcastTetraTransferEncoder: Encoder[ToplRpc.Transaction.BroadcastTetraTransfer.Params] =
    deriveEncoder
}

trait AdminRpcParamsEncoders extends SharedCodecs {

  implicit val unlockKeyfileParamsEncoder: Encoder[ToplRpc.Admin.UnlockKeyfile.Params] =
    deriveEncoder

  implicit val lockKeyfileParamsEncoder: Encoder[ToplRpc.Admin.LockKeyfile.Params] =
    deriveEncoder

  implicit val generateKeyfileParamsEncoder: Encoder[ToplRpc.Admin.GenerateKeyfile.Params] =
    deriveEncoder

  implicit val importSeedPhraseParamsEncoder: Encoder[ToplRpc.Admin.ImportSeedPhrase.Params] =
    deriveEncoder

  implicit val listOpenKeyfilesParamsEncoder: Encoder[ToplRpc.Admin.ListOpenKeyfiles.Params] =
    deriveEncoder

  implicit val startForgingParamsEncoder: Encoder[ToplRpc.Admin.StartForging.Params] =
    deriveEncoder

  implicit val stopForgingParamsEncoder: Encoder[ToplRpc.Admin.StopForging.Params] =
    deriveEncoder

  implicit val updateRewardsAddressParamsEncoder: Encoder[ToplRpc.Admin.UpdateRewardsAddress.Params] =
    deriveEncoder

  implicit val getRewardsAddressParamsEncoder: Encoder[ToplRpc.Admin.GetRewardsAddress.Params] =
    deriveEncoder

  implicit val statusParamsEncoder: Encoder[ToplRpc.Admin.Status.Params] =
    deriveEncoder
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

  implicit def nodeViewHeadInfoResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.HeadInfo.Response] =
    deriveDecoder

  implicit def nodeViewTransactionByIdResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.TransactionById.Response] =
    a =>
      for {
        tx          <- a.as[Transaction.TX]
        blockNumber <- a.get[Long]("blockNumber")
        blockId     <- a.get[ModifierId]("blockId")
      } yield ToplRpc.NodeView.TransactionById.Response(tx, blockNumber, blockId)

  implicit val nodeViewInfoResponseDecoder: Decoder[ToplRpc.NodeView.Info.Response] =
    deriveDecoder

  implicit val nodeViewBalancesResponseEntryBalancesDecoder: Decoder[ToplRpc.NodeView.Balances.EntryBalances] =
    deriveDecoder

  implicit val nodeViewBalancesResponseEntryBoxesDecoder: Decoder[ToplRpc.NodeView.Balances.EntryBoxes] =
    c =>
      for {
        polyBox  <- c.getOrElse[List[PolyBox]](PolyBox.typeString)(Nil)
        arbitBox <- c.getOrElse[List[ArbitBox]](ArbitBox.typeString)(Nil)
        assetBox <- c.getOrElse[List[AssetBox]](AssetBox.typeString)(Nil)
      } yield ToplRpc.NodeView.Balances.EntryBoxes(polyBox, arbitBox, assetBox)

  implicit val nodeViewBalancesResponseEntryDecoder: Decoder[ToplRpc.NodeView.Balances.Entry] =
    deriveDecoder

  implicit def nodeViewBalancesResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.Balances.Response] =
    Decoder.decodeMap[Address, ToplRpc.NodeView.Balances.Entry]

  implicit val nodeViewConfirmationStatusTxStatusDecoder: Decoder[ToplRpc.NodeView.ConfirmationStatus.TxStatus] =
    deriveDecoder

  implicit def nodeViewConfirmationStatusResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.ConfirmationStatus.Response] =
    Decoder.decodeMap[ModifierId, ToplRpc.NodeView.ConfirmationStatus.TxStatus]
}

trait TransactionRpcResponseDecoders extends SharedCodecs {

  implicit def transactionRawAssetTransferResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawAssetTransfer.Response] =
    json =>
      for {
        rawTx         <- json.get[AssetTransfer[Proposition]]("rawTx")
        messageToSign <- json.get[String]("messageToSign")
      } yield ToplRpc.Transaction.RawAssetTransfer.Response(rawTx, messageToSign)

  implicit def transactionRawArbitTransferResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawArbitTransfer.Response] =
    deriveDecoder

  implicit def transactionRawPolyTransferResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawPolyTransfer.Response] =
    deriveDecoder

  implicit def transactionEncodeTransferResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.EncodeTransfer.Response] =
    deriveDecoder

  implicit val transactionUnprovenPolyTransferResponseDecoder: Decoder[
    ToplRpc.Transaction.UnprovenPolyTransfer.Response
  ] = unprovenTransactionJsonDecoder.map(x => ToplRpc.Transaction.UnprovenPolyTransfer.Response(x))

  implicit val transactionBroadcastTetraTransferResponseDecoder: Decoder[
    ToplRpc.Transaction.BroadcastTetraTransfer.Response
  ] = hcursor => hcursor.as[Transaction.TX]
}

trait AdminRpcResponseDecoders extends SharedCodecs {

  implicit def unlockKeyfileResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Admin.UnlockKeyfile.Response] =
    Decoder.decodeMap

  // This implicit has the same type signature as the above codec
  //  implicit def lockKeyfileResponseDecoder(implicit
  //    networkPrefix: NetworkPrefix
  //  ): Decoder[ToplRpc.Admin.LockKeyfile.Response] =
  //    Decoder.decodeMap

  implicit def generateKeyfileResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Admin.GenerateKeyfile.Response] =
    deriveDecoder

  implicit def importSeedPhraseResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Admin.ImportSeedPhrase.Response] =
    deriveDecoder

  implicit def listOpenKeyfilesResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Admin.ListOpenKeyfiles.Response] =
    deriveDecoder

  implicit val startForgingResponseDecoder: Decoder[ToplRpc.Admin.StartForging.Response] =
    deriveDecoder

  implicit val stopForgingResponseDecoder: Decoder[ToplRpc.Admin.StopForging.Response] =
    deriveDecoder

  implicit val updateRewardsAddressResponseDecoder: Decoder[ToplRpc.Admin.UpdateRewardsAddress.Response] =
    deriveDecoder

  implicit val getRewardsAddressResponseDecoder: Decoder[ToplRpc.Admin.GetRewardsAddress.Response] =
    deriveDecoder

  implicit val statusResponseDecoder: Decoder[ToplRpc.Admin.Status.Response] =
    deriveDecoder
}

trait ToplRpcServerCodecs
    extends DebugRpcParamsDecoders
    with UtilRpcParamsDecoders
    with NodeViewRpcParamsDecoders
    with TransactionRpcParamsDecoders
    with AdminRpcParamsDecoders
    with DebugRpcResponseEncoders
    with UtilRpcResponseEncoders
    with NodeViewRpcResponseEncoders
    with TransactionRpcResponseEncoders
    with AdminRpcResponseEncoders

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

  implicit val nodeViewHeadInfoParamsDecoder: Decoder[ToplRpc.NodeView.HeadInfo.Params] =
    deriveDecoder

  implicit def nodeViewBalancesParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.Balances.Params] =
    deriveDecoder

  implicit val nodeViewTransactionsByIdParamsDecoder: Decoder[ToplRpc.NodeView.TransactionById.Params] =
    deriveDecoder

  implicit val nodeViewBlockByIdParamsDecoder: Decoder[ToplRpc.NodeView.BlockById.Params] =
    deriveDecoder

  implicit val nodeViewBlocksByIdsParamsDecoder: Decoder[ToplRpc.NodeView.BlocksByIds.Params] =
    deriveDecoder

  implicit val nodeViewBlocksByHeightParamsDecoder: Decoder[ToplRpc.NodeView.BlockByHeight.Params] =
    deriveDecoder

  implicit val nodeViewBlocksInRangeParamsDecoder: Decoder[ToplRpc.NodeView.BlocksInRange.Params] =
    deriveDecoder

  implicit val nodeViewBlockIdsInRangeParamsDecoder: Decoder[ToplRpc.NodeView.BlockIdsInRange.Params] =
    deriveDecoder

  implicit val nodeViewLatestBlocksParamsDecoder: Decoder[ToplRpc.NodeView.LatestBlocks.Params] =
    deriveDecoder

  implicit val nodeViewLatestBlockIdsParamsDecoder: Decoder[ToplRpc.NodeView.LatestBlockIds.Params] =
    deriveDecoder

  implicit val nodeViewMempoolParamsDecoder: Decoder[ToplRpc.NodeView.Mempool.Params] =
    deriveDecoder

  implicit val nodeViewTransactionFromMempoolParamsDecoder: Decoder[ToplRpc.NodeView.TransactionFromMempool.Params] =
    deriveDecoder

  implicit def nodeViewConfirmationStatusParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.NodeView.ConfirmationStatus.Params] =
    deriveDecoder

  implicit val nodeViewInfoParamsDecoder: Decoder[ToplRpc.NodeView.Info.Params] =
    deriveDecoder
}

trait TransactionRpcParamsDecoders extends SharedCodecs {

  // require custom decoders here to provide a default box selection algorithm

  implicit def transactionRawAssetTransferParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawAssetTransfer.Params] =
    cursor =>
      for {
        propositionType      <- cursor.downField("propositionType").as[String]
        sender               <- cursor.downField("sender").as[NonEmptyChain[Address]]
        recipients           <- cursor.downField("recipients").as[NonEmptyChain[(Address, AssetValue)]]
        fee                  <- cursor.downField("fee").as[Int128]
        changeAddress        <- cursor.downField("changeAddress").as[Address]
        consolidationAddress <- cursor.downField("consolidationAddress").as[Address]
        minting              <- cursor.downField("minting").as[Boolean]
        data                 <- cursor.downField("data").as[Option[Latin1Data]]
        boxSelectionAlgorithm <- cursor.getOrElse("boxSelectionAlgorithm")(
          BoxSelectionAlgorithms.All: BoxSelectionAlgorithm // default to BoxSelectionAlgorithms.All
        )
      } yield ToplRpc.Transaction.RawAssetTransfer.Params(
        propositionType,
        sender,
        recipients,
        fee,
        changeAddress,
        consolidationAddress,
        minting,
        data,
        boxSelectionAlgorithm
      )

  implicit def transactionRawArbitTransferParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawArbitTransfer.Params] =
    cursor =>
      for {
        propositionType      <- cursor.downField("propositionType").as[String]
        sender               <- cursor.downField("sender").as[NonEmptyChain[Address]]
        recipients           <- cursor.downField("recipients").as[NonEmptyChain[(Address, Int128)]]
        fee                  <- cursor.downField("fee").as[Int128]
        changeAddress        <- cursor.downField("changeAddress").as[Address]
        consolidationAddress <- cursor.downField("consolidationAddress").as[Address]
        data                 <- cursor.downField("data").as[Option[Latin1Data]]
        boxSelectionAlgorithm <- cursor.getOrElse("boxSelectionAlgorithm")(
          BoxSelectionAlgorithms.All: BoxSelectionAlgorithm // default to BoxSelectionAlgorithms.All
        )
      } yield ToplRpc.Transaction.RawArbitTransfer.Params(
        propositionType,
        sender,
        recipients,
        fee,
        changeAddress,
        consolidationAddress,
        data,
        boxSelectionAlgorithm
      )

  implicit def transactionRawPolyTransferParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.RawPolyTransfer.Params] = deriveDecoder

  implicit val transactionUnprovenPolyTransferParamsDecoder: Decoder[ToplRpc.Transaction.UnprovenPolyTransfer.Params] =
    deriveDecoder

  implicit val transactionUnprovenArbitTransferParamsDecoder
    : Decoder[ToplRpc.Transaction.UnprovenArbitTransfer.Params] =
    deriveDecoder

  implicit val transactionUnprovenAssetTransferParamsDecoder
    : Decoder[ToplRpc.Transaction.UnprovenAssetTransfer.Params] =
    deriveDecoder

  implicit def transactionBroadcastTxParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.BroadcastTx.Params] =
    Decoder.forProduct1("tx")(ToplRpc.Transaction.BroadcastTx.Params.apply)

  implicit val transactionBroadcastTetraTransferParamsDecoder
    : Decoder[ToplRpc.Transaction.BroadcastTetraTransfer.Params] =
    deriveDecoder

  implicit def transactionEncodeTransferParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Transaction.EncodeTransfer.Params] =
    Decoder.forProduct1("unprovenTransaction")(ToplRpc.Transaction.EncodeTransfer.Params.apply)
}

trait AdminRpcParamsDecoders extends SharedCodecs {

  implicit val unlockKeyfileParamsDecoder: Decoder[ToplRpc.Admin.UnlockKeyfile.Params] =
    deriveDecoder

  implicit def lockKeyfileParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Admin.LockKeyfile.Params] =
    deriveDecoder

  implicit val generateKeyfileParamsDecoder: Decoder[ToplRpc.Admin.GenerateKeyfile.Params] =
    deriveDecoder

  implicit val importSeedPhraseParamsDecoder: Decoder[ToplRpc.Admin.ImportSeedPhrase.Params] =
    deriveDecoder

  implicit val listOpenKeyfilesParamsDecoder: Decoder[ToplRpc.Admin.ListOpenKeyfiles.Params] =
    deriveDecoder

  implicit val startForgingParamsDecoder: Decoder[ToplRpc.Admin.StartForging.Params] =
    deriveDecoder

  implicit val stopForgingParamsDecoder: Decoder[ToplRpc.Admin.StopForging.Params] =
    deriveDecoder

  implicit def updateRewardsAddressParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Admin.UpdateRewardsAddress.Params] =
    deriveDecoder

  implicit val getRewardsAddressParamsDecoder: Decoder[ToplRpc.Admin.GetRewardsAddress.Params] =
    deriveDecoder

  implicit val statusParamsDecoder: Decoder[ToplRpc.Admin.Status.Params] =
    deriveDecoder
}

trait DebugRpcResponseEncoders extends SharedCodecs {

  implicit val debugDelayResponseEncoder: Encoder[ToplRpc.Debug.Delay.Response] =
    deriveEncoder

  implicit val debugMyBlocksResponseEncoder: Encoder[ToplRpc.Debug.MyBlocks.Response] =
    deriveEncoder

  implicit val debugGeneratorsResponseEncoder: Encoder[ToplRpc.Debug.Generators.Response] =
    r =>
      r.map { case (address, count) =>
        addressJsonKeyEncoder(address) -> count
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

  implicit val nodeViewHeadInfoResponseEncoder: Encoder[ToplRpc.NodeView.HeadInfo.Response] =
    deriveEncoder

  implicit val nodeViewBalancesResponseEntryEncoder: Encoder[ToplRpc.NodeView.Balances.Entry] =
    deriveEncoder

  implicit val nodeViewBalancesResponseEntryBalancesEncoder: Encoder[ToplRpc.NodeView.Balances.EntryBalances] =
    deriveEncoder

  implicit val nodeViewBalancesResponseEntryBoxesEncoder: Encoder[ToplRpc.NodeView.Balances.EntryBoxes] =
    deriveEncoder

  implicit val nodeViewBalancesResponseEncoder: Encoder[ToplRpc.NodeView.Balances.Response] =
    _.map { case (address, entry) =>
      addressJsonKeyEncoder(address) -> entry
    }.asJson

  implicit val nodeViewTransactionByIdResponseEncoder: Encoder[ToplRpc.NodeView.TransactionById.Response] =
    r =>
      Map(
        "blockNumber" -> r.blockNumber.asJson,
        "blockId"     -> r.blockId.asJson
      ).asJson.deepMerge(r.transaction.asJson)

  implicit val nodeViewInfoResponseEncoder: Encoder[ToplRpc.NodeView.Info.Response] =
    deriveEncoder

  implicit val nodeViewConfirmationStatusTxStatusEncoder: Encoder[ToplRpc.NodeView.ConfirmationStatus.TxStatus] =
    deriveEncoder

  implicit val nodeViewConfirmationStatusResponseEncoder: Encoder[ToplRpc.NodeView.ConfirmationStatus.Response] =
    Encoder.encodeMap
}

trait TransactionRpcResponseEncoders extends SharedCodecs {

  implicit val transactionRawAssetTransferResponseEncoder: Encoder[ToplRpc.Transaction.RawAssetTransfer.Response] =
    deriveEncoder

  implicit val transactionRawArbitTransferResponseEncoder: Encoder[ToplRpc.Transaction.RawArbitTransfer.Response] =
    deriveEncoder

  implicit val transactionRawPolyTransferResponseEncoder: Encoder[ToplRpc.Transaction.RawPolyTransfer.Response] =
    deriveEncoder

  implicit val transactionUnprovenPolyTransferResponseEncoder
    : Encoder[ToplRpc.Transaction.UnprovenPolyTransfer.Response] = deriveEncoder

  implicit val transactionEncodeTransferResponseEncoder: Encoder[ToplRpc.Transaction.EncodeTransfer.Response] =
    deriveEncoder
}

trait AdminRpcResponseEncoders extends SharedCodecs {

  implicit val unlockKeyfileResponseEncoder: Encoder[ToplRpc.Admin.UnlockKeyfile.Response] =
    Encoder.encodeMap

  // This implicit has the same type signature as the above codec
  //  implicit val lockKeyfileResponseEncoder: Encoder[ToplRpc.Admin.LockKeyfile.Response] =
  //    Encoder.encodeMap

  implicit val generateKeyfileResponseEncoder: Encoder[ToplRpc.Admin.GenerateKeyfile.Response] =
    deriveEncoder

  implicit val importSeedPhraseResponseEncoder: Encoder[ToplRpc.Admin.ImportSeedPhrase.Response] =
    deriveEncoder

  implicit val listOpenKeyfilesResponseEncoder: Encoder[ToplRpc.Admin.ListOpenKeyfiles.Response] =
    deriveEncoder

  implicit val startForgingResponseEncoder: Encoder[ToplRpc.Admin.StartForging.Response] =
    deriveEncoder

  implicit val stopForgingResponseEncoder: Encoder[ToplRpc.Admin.StopForging.Response] =
    deriveEncoder

  implicit val updateRewardsAddressResponseEncoder: Encoder[ToplRpc.Admin.UpdateRewardsAddress.Response] =
    deriveEncoder

  implicit val getRewardsAddressResponseEncoder: Encoder[ToplRpc.Admin.GetRewardsAddress.Response] =
    deriveEncoder

  implicit val statusResponseEncoder: Encoder[ToplRpc.Admin.Status.Response] =
    deriveEncoder
}

trait SharedCodecs extends JsonCodecs with ModelsJsonCodecs {

  implicit def assetTransferDecoder(implicit networkPrefix: NetworkPrefix): Decoder[AssetTransfer[Proposition]] =
    Decoder
      .instance(c => assetTransferJsonDecoder.apply(c))
      .map { case a: AssetTransfer[Proposition @unchecked] =>
        a
      }

  implicit def arbitTransferDecoder(implicit networkPrefix: NetworkPrefix): Decoder[ArbitTransfer[Proposition]] =
    Decoder
      .instance(c => arbitTransferJsonDecoder.apply(c))
      .map { case a: ArbitTransfer[Proposition @unchecked] =>
        a
      }

  implicit def polyTransferDecoder(implicit networkPrefix: NetworkPrefix): Decoder[PolyTransfer[Proposition]] =
    Decoder
      .instance(c => polyTransferJsonDecoder.apply(c))
      .map { case a: PolyTransfer[Proposition @unchecked] =>
        a
      }
}
