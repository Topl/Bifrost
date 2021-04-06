package co.topl.http.rpc

import co.topl.utils.NetworkType.NetworkPrefix
import io.circe._
import io.circe.generic.semiauto._

trait ToplRpcClientCodecs {

  // Parameter Encoders

  implicit val debugDelayParamsEncoder: Encoder[ToplRpc.Debug.Delay.Params] =
    Encoder.forProduct2("blockId", "numBlocks")(p => (p.blockId, p.numBlocks))

  implicit val debugMyBlocksParamsEncoder: Encoder[ToplRpc.Debug.MyBlocks.Params] =
    deriveEncoder

  implicit val debugGeneratorsParamsEncoder: Encoder[ToplRpc.Debug.Generators.Params] =
    deriveEncoder

  implicit val debugIdsFromHeightParamsEncoder: Encoder[ToplRpc.Debug.IdsFromHeight.Params] =
    deriveEncoder

  implicit val utilsSeedParamsEncoder: Encoder[ToplRpc.Util.Seed.Params] =
    deriveEncoder

  implicit val utilsSeedOfLengthParamsEncoder: Encoder[ToplRpc.Util.SeedOfLength.Params] =
    deriveEncoder

  implicit val utilsHashBlake2b256ParamsEncoder: Encoder[ToplRpc.Util.HashBlake2b256.Params] =
    deriveEncoder

  implicit val utilsGenerateAssetCodeParamsEncoder: Encoder[ToplRpc.Util.GenerateAssetCode.Params] =
    Encoder.forProduct3("version", "issuer", "shortName")(p => (p.version, p.issuer, p.shortName))

  implicit val utilsCheckValidAddressParamsEncoder: Encoder[ToplRpc.Util.CheckValidAddress.Params] =
    Encoder.forProduct2("network", "address")(p => (p.network, p.address))

  // Response Decoders

  implicit val debugDelayResponseDecoder: Decoder[ToplRpc.Debug.Delay.Response] =
    deriveDecoder

  implicit def debugMyBlocksResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Debug.MyBlocks.Response] =
    Decoder.forProduct2("pubkeys", "count")(ToplRpc.Debug.MyBlocks.Response.apply)

  implicit val debugIdsFromHeightResponseDecoder: Decoder[ToplRpc.Debug.IdsFromHeight.Response] =
    _.as[ToplRpc.Debug.IdsFromHeight.Response]

  implicit val utilsSeedResponseDecoder: Decoder[ToplRpc.Util.Seed.Response] =
    deriveDecoder

  implicit val utilsSeedOfLengthResponseDecoder: Decoder[ToplRpc.Util.SeedOfLength.Response] =
    deriveDecoder

  implicit val utilsHashBlake2b256ResponseDecoder: Decoder[ToplRpc.Util.HashBlake2b256.Response] =
    deriveDecoder

  implicit val utilsGenerateAssetCodeResponseDecoder: Decoder[ToplRpc.Util.GenerateAssetCode.Response] =
    Decoder.forProduct1("assetCode")(ToplRpc.Util.GenerateAssetCode.Response.apply)

  implicit def utilsCheckValidAddressResponseDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ToplRpc.Util.CheckValidAddress.Response] =
    Decoder.forProduct2("address", "network")(ToplRpc.Util.CheckValidAddress.Response.apply)

}
