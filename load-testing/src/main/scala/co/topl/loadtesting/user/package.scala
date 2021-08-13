package co.topl.loadtesting

import co.topl.akkahttprpc.RpcClientFailure

package object user {

  /**
   * Indicates that a failure has occurred while attempting to send Polys.
   */
  sealed trait SendPolysFailure

  /**
   * Indicates that the user's balance does not have enough Polys to complete a Poly TX.
   * @param numPolys the number of Polys currently in this user's balance
   */
  case class NotEnoughPolysForPolyTx(numPolys: Int) extends SendPolysFailure

  /**
   * Indicates that the user does not have any contacts to send Polys to.
   */
  case object NoContacts extends SendPolysFailure

  /**
   * Indicates that an RPC request has failed while attempting to send Polys.
   * @param failure the error message
   */
  case class PolysRpcFailure(failure: RpcClientFailure) extends SendPolysFailure

  /**
   * Indicates that a failure has occurred while attempting to send Assets.
   */
  sealed trait SendAssetsFailure

  /**
   * Indicates that the user's balance does not have enough Polys to complete an Asset TX.
   * @param numPolys the number of Polys currently in this user's balance
   */
  case class NotEnoughPolysForAssetTx(numPolys: Int) extends SendAssetsFailure

  /**
   * Indicates that an RPC request has failed while attempting to send Assets.
   * @param failure the error message
   */
  case class AssetsRpcFailure(failure: RpcClientFailure) extends SendAssetsFailure

  /**
   * The number of polys this user should send in a Poly TX.
   */
  private[user] val numberOfPolysToSend = 100

  /**
   * The number of polys required to begin a Poly TX operation.
   */
  private[user] val numberOfPolysRequiredForPolyOp = 300

  /**
   * The number of polys required to begin an Asset TX operation.
   */
  private[user] val numberOfPolysRequiredForAssetOp = 100
}
