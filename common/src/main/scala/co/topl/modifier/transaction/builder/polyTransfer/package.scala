package co.topl.modifier.transaction.builder

import cats.implicits._
import co.topl.attestation.{EvidenceProducer, Proposition}
import co.topl.modifier.transaction.PolyTransfer
import co.topl.utils.Identifiable

package object polyTransfer {
  sealed trait InvalidPolyTransfer
  case object InsufficientFunds extends InvalidPolyTransfer

  trait Instances {

    implicit val automaticPolyBuilder: PolyTransferBuildStrategy[AutomaticBuilder] =
      new PolyTransferBuildStrategy[AutomaticBuilder] {

        override def execute[P <: Proposition: EvidenceProducer: Identifiable](
          strategy: AutomaticBuilder
        ): Either[InvalidPolyTransfer, PolyTransfer[P]] = AutomaticBuilder.buildPolyTransfer(strategy)
      }

    implicit val customPolyBuilder: PolyTransferBuildStrategy[CustomBuilder] =
      new PolyTransferBuildStrategy[CustomBuilder] {

        override def execute[P <: Proposition: EvidenceProducer: Identifiable](
          strategy: CustomBuilder
        ): Either[InvalidPolyTransfer, PolyTransfer[P]] =
          CustomBuilder.buildPolyTransfer(strategy).asRight
      }
  }

  trait Implicits extends Instances with PolyTransferBuildStrategy.ToPolyTransferBuildStrategyOps

  object implicits extends Implicits
}
