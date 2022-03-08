package co.topl.modifier.transaction.builder.ops

import cats.implicits._
import cats.Foldable
import co.topl.models.Transaction

import scala.language.implicitConversions

class CoinOutputsOps(private val value: List[Transaction.CoinOutput]) {

  def splitByCoinType: (List[Transaction.PolyOutput], List[Transaction.ArbitOutput], List[Transaction.AssetOutput]) =
    value.foldLeft(
      (List.empty[Transaction.PolyOutput], List.empty[Transaction.ArbitOutput], List.empty[Transaction.AssetOutput])
    ) {
      case ((polys, arbits, assets), poly: Transaction.PolyOutput)   => (polys :+ poly, arbits, assets)
      case ((polys, arbits, assets), arbit: Transaction.ArbitOutput) => (polys, arbits :+ arbit, assets)
      case ((polys, arbits, assets), asset: Transaction.AssetOutput) => (polys, arbits, assets :+ asset)
    }
}

object CoinOutputsOps {

  trait ToCoinOutputOps {

    implicit def coinOutputOpsFromCoinOutputs[F[_]: Foldable](value: F[Transaction.CoinOutput]): CoinOutputsOps =
      new CoinOutputsOps(value.toList)

  }

  trait Implicits extends ToCoinOutputOps

  object implicits extends Implicits
}
