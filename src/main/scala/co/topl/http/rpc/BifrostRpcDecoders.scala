package co.topl.http.rpc

import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Decoder
import io.circe.generic.semiauto._

trait BifrostRpcDecoders {

  implicit val debugDelayParamsDecoder: Decoder[BifrostRpc.Debug.Delay.Params] =
    Decoder.forProduct2("blockId", "numBlocks")(BifrostRpc.Debug.Delay.Params.apply)

  implicit val debugMyBlocksParamsDecoder: Decoder[BifrostRpc.Debug.MyBlocks.Params] =
    deriveDecoder

  implicit val debugGeneratorsParamsDecoder: Decoder[BifrostRpc.Debug.Generators.Params] =
    deriveDecoder

  implicit val debugIdsFromHeightParamsDecoder: Decoder[BifrostRpc.Debug.IdsFromHeight.Params] =
    deriveDecoder

  implicit val utilsSeedParamsDecoder: Decoder[BifrostRpc.Utils.Seed.Params] =
    deriveDecoder

  implicit val utilsSeedOfLengthParamsDecoder: Decoder[BifrostRpc.Utils.SeedOfLength.Params] =
    deriveDecoder

  implicit val utilsHashBlake2b256ParamsDecoder: Decoder[BifrostRpc.Utils.HashBlake2b256.Params] =
    deriveDecoder

  implicit def utilsGenerateAssetCodeParamsDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[BifrostRpc.Utils.GenerateAssetCode.Params] =
    Decoder.forProduct3("version", "issuer", "shortName")(BifrostRpc.Utils.GenerateAssetCode.Params.apply)


  implicit def utilsCheckValidAddressParamsDecoder(implicit
                                                    networkPrefix: NetworkPrefix
                                                   ): Decoder[BifrostRpc.Utils.CheckValidAddress.Params] =
    Decoder.forProduct2("network", "address")(BifrostRpc.Utils.CheckValidAddress.Params.apply)

}
