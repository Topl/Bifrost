package co.topl.modifier.transaction.builder.tetra

import cats.Monad
import cats.data.{Chain, EitherT, NonEmptyChain}
import co.topl.models.Box.Values.Asset
import co.topl.models._
import co.topl.modifier.transaction.builder.BoxCache.BoxSet.ToBoxReferencesFailure
import co.topl.modifier.transaction.builder.tetra.TransferBuilder.BuildTransferFailure
import co.topl.modifier.transaction.builder.{BoxCache, BoxSelectionAlgorithm}

trait TransferBuilder[F[_]] {

  def build(
    fromAddresses:         Chain[DionAddress],
    feeOutput:             Option[Transaction.PolyOutput],
    outputs:               NonEmptyChain[Transaction.CoinOutput],
    fee:                   Int128,
    timestamp:             Timestamp,
    data:                  Option[TransactionData],
    minting:               Boolean,
    boxSelectionAlgorithm: BoxSelectionAlgorithm
  ): EitherT[F, BuildTransferFailure, Transaction.Unproven]
}

object TransferBuilder {

  sealed trait BuildTransferFailure

  object BuildTransferFailures {
    case object InsufficientPolys extends BuildTransferFailure
    case object InsufficientArbits extends BuildTransferFailure
    case class InsufficientAssets(assetCode: Asset.Code) extends BuildTransferFailure
    case class InvalidBoxReference(inner: ToBoxReferencesFailure) extends BuildTransferFailure
    case object EmptyInputs extends BuildTransferFailure
    case object EmptyOutputs extends BuildTransferFailure
  }

  def apply[F[_]: Monad](boxNonceCache: BoxCache[F]): TransferBuilder[F] = new TransferBuilderImpl[F](boxNonceCache)

}
