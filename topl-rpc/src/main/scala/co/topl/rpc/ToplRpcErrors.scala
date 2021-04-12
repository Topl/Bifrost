package co.topl.rpc

import cats.data.NonEmptyChain
import co.topl.akkahttprpc.{CustomError, ThrowableData}
import co.topl.modifier.transaction.SyntacticValidationFailure
import io.circe.Encoder
import io.circe.syntax._

object ToplRpcErrors {

  val NoBlockIdsAtHeight: CustomError = CustomError(-32000, "No block ids found from that block height")

  def FailedToGenerateAssetCode(throwable: Throwable)(implicit
    throwableEncoder:                      Encoder[ThrowableData]
  ): CustomError =
    CustomError.fromThrowable(-32001, "Failed to generate asset code", throwable)
  val InvalidNetworkSpecified: CustomError = CustomError(-32002, "Invalid network specified")

  def unsupportedOperation(reason: String): CustomError =
    CustomError(-32003, "Unsupported Operation", Map("reason" -> reason).asJson)

  def transactionValidationException(throwable: Throwable)(implicit
    throwableEncoder:                           Encoder[ThrowableData]
  ): CustomError =
    CustomError.fromThrowable(-32004, "Could not validate transaction", throwable)

  def syntacticValidationFailure(failures: NonEmptyChain[SyntacticValidationFailure]): CustomError =
    CustomError(
      -32005,
      "Transaction is not semantically valid",
      Map("failures" -> failures.map(_.toString).asJson).asJson
    )

  def genericFailure(reason: String): CustomError =
    CustomError(
      -32098,
      "Unknown failure",
      Map("reason" -> reason).asJson
    )

}
