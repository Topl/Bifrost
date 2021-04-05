package co.topl.http.rpc

import co.topl.akkahttprpc.CustomError

object BifrostRpcErrors {

  val NoBlockIdsAtHeight: CustomError = CustomError(-32000, "No block ids found from that block height", None)
  def FailedToGenerateAssetCode(throwable: Throwable): CustomError = CustomError(-32001, "Failed to generate asset code", None)
  val InvalidNetworkSpecified: CustomError = CustomError(-32002, "Invalid network specified", None)

}
