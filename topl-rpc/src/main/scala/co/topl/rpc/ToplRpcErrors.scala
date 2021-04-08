package co.topl.rpc

import cats.data.NonEmptyChain
import co.topl.akkahttprpc.{CustomError, ThrowableData}
import co.topl.modifier.transaction.{Syntactic, SyntacticValidationFailure}
import io.circe.Encoder
import io.circe.syntax._

object ToplRpcErrors {

  val NoBlockIdsAtHeight: CustomError = CustomError(-32000, "No block ids found from that block height", None)

  def FailedToGenerateAssetCode(throwable: Throwable)(implicit
    throwableEncoder:                      Encoder[ThrowableData]
  ): CustomError =
    CustomError.fromThrowable(-32001, "Failed to generate asset code", throwable)
  val InvalidNetworkSpecified: CustomError = CustomError(-32002, "Invalid network specified", None)

  def unsupportedOperation(reason: String): CustomError =
    CustomError(-32003, "Unsupported Operation", Some(Map("reason" -> reason).asJson))

  def transactionValidationException(throwable: Throwable)(implicit
    throwableEncoder:                           Encoder[ThrowableData]
  ): CustomError =
    CustomError.fromThrowable(-32004, "Could not validate transaction", throwable)

  def syntacticValidationFailure(failures: NonEmptyChain[SyntacticValidationFailure]): CustomError =
    CustomError(
      -32005,
      "Transaction is not semantically valid",
      Some(Map("failures" -> failures.map(_.toString).asJson).asJson)
    )

}
