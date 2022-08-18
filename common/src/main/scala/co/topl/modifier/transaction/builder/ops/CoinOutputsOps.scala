package co.topl.modifier.transaction.builder.ops

import cats.implicits._
import cats.Foldable
import co.topl.models._

import scala.language.implicitConversions

class CoinOutputsOps(private val value: List[Transaction.Output]) {

  def splitByCoinType: (List[Transaction.Output], List[Transaction.Output], List[Transaction.Output]) =
    value.foldLeft(
      (List.empty[Transaction.Output], List.empty[Transaction.Output], List.empty[Transaction.Output])
    ) {
      case ((polys, arbits, assets), poly @ Transaction.Output(_, _: Box.Values.Poly, _)) =>
        (polys :+ poly, arbits, assets)
      case ((polys, arbits, assets), arbit @ Transaction.Output(_, _: Box.Values.Arbit, _)) =>
        (polys, arbits :+ arbit, assets)
      case ((polys, arbits, assets), asset @ Transaction.Output(_, _: Box.Values.AssetV1, _)) =>
        (polys, arbits, assets :+ asset)
    }
}

object CoinOutputsOps {

  trait ToCoinOutputOps {

    implicit def coinOutputOpsFromCoinOutputs[F[_]: Foldable](value: F[Transaction.Output]): CoinOutputsOps =
      new CoinOutputsOps(value.toList)

  }

  trait Implicits extends ToCoinOutputOps

  object implicits extends Implicits
}
