package co.topl.consensus

import cats.MonadError
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState}
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.hash.blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{Lengths, Sized}
import co.topl.typeclasses.implicits._

object EtaCalculation {

  object Eval {

    def make[F[_]: MonadError[*[_], Throwable]](
      state: ConsensusState[F],
      clock: ClockAlgebra[F]
    ): EtaCalculationAlgebra[F] =
      new EtaCalculationAlgebra[F] {

        override def calculate(epoch: Epoch): F[Eta] =
          for {
            epochN1 <- (epoch - 1).pure[F]
            previousEta <- OptionT(state.lookupEta(epochN1))
              .getOrElseF(new IllegalStateException(s"Unknown Eta for epoch=$epochN1").raiseError[F, Eta])
            genesis    <- state.genesis
            head       <- state.canonicalHead
            epochRange <- clock.epochRange(epoch)
            rhoValues: List[Rho] <-
              if (head === genesis) {
                if (epoch === 0L)
                  List(ProofToHash.digest(genesis.headerV2.eligibilityCertificate.vrfNonceSig)).pure[F]
                else Nil.pure[F]
              } else {
                gatherRhoValues(genesis.headerV2, head.headerV2, epochRange)
              }
            nextEta <- calculate(previousEta, epoch, rhoValues)
          } yield nextEta

        private def gatherRhoValues(
          genesis:    BlockHeaderV2,
          head:       BlockHeaderV2,
          epochRange: ClockAlgebra.EpochBoundary
        ) =
          List(
            (
              head.parentHeaderId,
              ProofToHash.digest(head.eligibilityCertificate.vrfNonceSig),
              head.slot
            )
          ).iterateWhileM(acc =>
            OptionT(state.lookupBlockHeader(acc.head._1))
              .map(h => (h.parentHeaderId, ProofToHash.digest(h.eligibilityCertificate.vrfNonceSig), h.slot))
              .getOrElseF(
                new IllegalStateException(s"Unknown Block id=${acc.head._1}")
                  .raiseError[F, (TypedIdentifier, Rho, Slot)]
              )
              .map(_ :: acc)
          )(blocks => blocks.head._3 >= epochRange.start && blocks.head._1 =!= genesis.id)
            .map(_.filter { case (_, _, slot) =>
              slot >= epochRange.start && (slot - epochRange.start) <= (epochRange.length * 2 / 3)
            }.map(_._2))

        private def calculate(previousEta: Eta, epoch: Epoch, rhoValues: Iterable[Rho]): F[Eta] =
          Sized
            .strictUnsafe[Bytes, Lengths.`32`.type](
              Bytes(
                blake2b256
                  .hash(
                    None,
                    EtaCalculationArgs(previousEta, epoch, rhoValues).digestMessages: _*
                  )
                  .value
              )
            )
            .pure[F]
      }
  }
}

private case class EtaCalculationArgs(previousEta: Eta, epoch: Epoch, rhoValues: Iterable[Rho]) {

  def digestMessages: List[Array[Byte]] =
    (List(previousEta.data) ++ List(Bytes(BigInt(epoch).toByteArray)) ++ rhoValues
      .map(_.data))
      .map(_.toArray)
}
