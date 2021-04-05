package co.topl.http.rpc

import cats.implicits._
import io.circe.Decoder
import io.circe.generic.semiauto._

trait BifrostRpcDecoders {

  implicit val debugDelayParamsDecoder: Decoder[BifrostRpc.Debug.Delay.Params] =
    Decoder.forProduct2("blockId", "numBlocks")(BifrostRpc.Debug.Delay.Params.apply)

  implicit val debugMyBlocksParamsDecoder: Decoder[BifrostRpc.Debug.MyBlocks.Params] =
    _ => BifrostRpc.Debug.MyBlocks.Params().asRight

  implicit val debugGeneratorsParamsDecoder: Decoder[BifrostRpc.Debug.Generators.Params] =
    _ => BifrostRpc.Debug.Generators.Params().asRight

  implicit val debugIdsFromHeightParamsDecoder: Decoder[BifrostRpc.Debug.IdsFromHeight.Params] =
    deriveDecoder
}
