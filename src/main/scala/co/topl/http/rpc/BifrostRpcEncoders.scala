package co.topl.http.rpc

import co.topl.attestation.Address
import io.circe._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._

trait BifrostRpcEncoders {

  implicit val debugDelayResponseEncoder: Encoder[BifrostRpc.Debug.Delay.Response] =
    deriveEncoder

  implicit val debugMyBlocksResponseEncoder: Encoder[BifrostRpc.Debug.MyBlocks.Response] =
    Encoder.forProduct2("pubkeys", "count")(r => (r.pubkeys, r.count))

  implicit val debugGeneratorsResponseEncoder: Encoder[BifrostRpc.Debug.Generators.Response] =
    r =>
      r.map { case (address, count) =>
        Address.jsonKeyEncoder(address) -> count
      }.asJson

  implicit val debugIdsFromHeightResponseEncoder: Encoder[BifrostRpc.Debug.IdsFromHeight.Response] =
    _.asJson

  implicit val utilsSeedResponseEncoder: Encoder[BifrostRpc.Utils.Seed.Response] =
    deriveEncoder

  implicit val utilsSeedOfLengthResponseEncoder: Encoder[BifrostRpc.Utils.SeedOfLength.Response] =
    deriveEncoder

  implicit val utilsHashBlake2b256ResponseEncoder: Encoder[BifrostRpc.Utils.HashBlake2b256.Response] =
    deriveEncoder

  implicit val utilsGenerateAssetCodeResponseEncoder: Encoder[BifrostRpc.Utils.GenerateAssetCode.Response] =
    Encoder.forProduct1("assetCode")(_.assetCode)

  implicit val utilsCheckValidAddressResponseEncoder: Encoder[BifrostRpc.Utils.CheckValidAddress.Response] =
    Encoder.forProduct2("address", "network")(r => (r.address, r.network))
    
}
