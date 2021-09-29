package co.topl.consensus

import cats.data.OptionT
import cats.implicits._
import cats.{Foldable, MonadError}
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{BlockchainState, ClockAlgebra}
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
      state: BlockchainState[F],
      clock: ClockAlgebra[F]
    ): EtaCalculationAlgebra[F] =
      new EtaCalculationAlgebra[F] {

        override def calculate(epoch: Epoch): F[Eta] =
          for {
            epochN1 <- (epoch - 1).pure[F]
            eta <- OptionT(state.lookupEta(epochN1))
              .getOrElseF(new IllegalStateException(s"Unknown Eta for epoch=$epochN1").raiseError[F, Eta])
            genesis    <- state.genesis
            head       <- state.canonicalHead
            epochStart <- clock.epochRange(epoch).map(_.start)
            blockProofHashes <- List(
              (
                head.headerV2.parentHeaderId,
                ProofToHash.digest(head.headerV2.eligibibilityCertificate.vrfNonceSig),
                head.headerV2.slot
              )
            ).iterateWhileM(acc =>
              OptionT(state.lookupBlock(acc.head._1))
                .map(_.headerV2)
                .map(h => (h.parentHeaderId, ProofToHash.digest(h.eligibibilityCertificate.vrfNonceSig), h.slot))
                .getOrElseF(
                  new IllegalStateException(s"Unknown Block id=${acc.head._1}")
                    .raiseError[F, (TypedIdentifier, Rho, Slot)]
                )
                .map(_ :: acc)
            )(blocks => blocks.head._3 >= epochStart && blocks.head._1 =!= genesis.headerV2.id)
              .map(_.map(_._2))
            nextEta <- calculate(eta, blockProofHashes)
          } yield nextEta

        private def calculate[C[_]: Foldable](previousEta: Eta, rhoValues: C[Rho]): F[Eta] =
          Sized
            .strictUnsafe[Bytes, Lengths.`32`.type](
              Bytes(
                blake2b256
                  .hash(None, (List(previousEta.data) ++ rhoValues.toIterable.map(_.data)).map(_.toArray): _*)
                  .value
              )
            )
            .pure[F]
      }
  }
}
