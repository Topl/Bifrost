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
}
