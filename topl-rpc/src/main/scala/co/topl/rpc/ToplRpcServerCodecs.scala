package co.topl.rpc

import co.topl.attestation.Address
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import io.circe.syntax._

trait ToplRpcServerCodecs {

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

}
