package co.topl.modifier.transaction.builder.tetra

import cats.Monad
import cats.data.{Chain, EitherT, NonEmptyChain}
import cats.implicits._
import co.topl.attestation.{Address, Evidence}
import co.topl.models.{DionAddress, Int128 => TetraInt128, Timestamp, Transaction, TransactionData}
import co.topl.modifier.box.AssetCode
import co.topl.modifier.implicits._
import co.topl.modifier.transaction.builder.BoxCache.BoxSet
import co.topl.modifier.transaction.builder.{BoxCache, BoxSelectionAlgorithm}
import co.topl.utils.Int128

class TransferBuilderImpl[F[_]: Monad](private val boxCache: BoxCache[F]) extends TransferBuilder[F] {

  import TransferBuilder._

  override def build(
    fromAddresses:         Chain[DionAddress],
    feeOutput:             Option[Transaction.PolyOutput],
    outputs:               NonEmptyChain[Transaction.CoinOutput],
    fee:                   TetraInt128,
    timestamp:             Timestamp,
    data:                  Option[TransactionData],
    minting:               Boolean,
    boxSelectionAlgorithm: BoxSelectionAlgorithm
  ): EitherT[F, BuildTransferFailure, Transaction.Unproven] =
    for {
      addresses <-
        fromAddresses
          .map(dionAddress =>
            Address(Evidence(dionAddress.typedEvidence.allBytes.toArray))(dionAddress.networkPrefix.value)
          )
          .asRight[BuildTransferFailure]
          .toEitherT[F]
      boxes <- EitherT.right(boxCache.get(addresses.toList))
      // calculate the needed token amounts from the outputs
      polysNeeded =
        outputs.toChain.foldLeft(Int128(0)) {
          case (sum, Transaction.PolyOutput(_, amount)) => sum + amount.data
          case (sum, _)                                 => sum
        }
      arbitsNeeded =
        outputs.toChain.foldLeft(Int128(0)) {
          case (sum, Transaction.ArbitOutput(_, amount)) => sum + amount.data
          case (sum, _)                                  => sum
        }
      assetsNeeded =
        outputs.toChain.foldLeft(Chain.empty[(AssetCode, Int128)]) {
          case (assetSums, Transaction.AssetOutput(_, value)) =>
            val assetValue = value.toAssetValue

            // update the asset sums with the quantity of this asset added
            assetSums
              .find(_._1 == assetValue.assetCode)
              .map(asset => asset._1 -> (asset._2 + assetValue.quantity))
              .fold(assetSums.append(assetValue.assetCode -> assetValue.quantity))(x =>
                assetSums.map(pair =>
                  if (pair._1 == assetValue.assetCode) x
                  else pair
                )
              )
          case (sum, _) => sum
        }
      boxReferences <-
        BoxSet
          .toBoxReferences(
            BoxSelectionAlgorithm.pickBoxes(
              boxSelectionAlgorithm,
              boxes,
              polysNeeded,
              arbitsNeeded,
              assetsNeeded
            )
          )
          // this error should never happen, but we will try to handle it anyways
          .leftMap(err => BuildTransferFailures.InvalidBoxReference(err): BuildTransferFailure)
          .toEitherT[F]
      unprovenTransaction =
        Transaction.Unproven(
          boxReferences,
          feeOutput,
          outputs,
          fee,
          timestamp,
          data,
          minting
        )
    } yield unprovenTransaction
}
