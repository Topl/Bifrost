package co.topl.demo

import cats._
import cats.implicits._
import co.topl.algebras.BlockchainState
import co.topl.models.Box.Values
import co.topl.models.utility.Ratio
import co.topl.models._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

object InMemoryState {

  object Eval {

    def make[F[_]: Applicative](
      genesisBlock:          BlockV2,
      initialBlocks:         Map[TypedIdentifier, BlockV2] = Map.empty,
      initialRelativeStakes: Map[Epoch, Map[TaktikosAddress, Ratio]] = Map.empty,
      initialRegistrations:  Map[Epoch, Map[TaktikosAddress, Box.Values.TaktikosRegistration]] = Map.empty,
      initialEtas:           Map[Epoch, Eta] = Map.empty
    ): BlockchainState[F] =
      new BlockchainState[F] {

        private var _blocks: Map[TypedIdentifier, BlockV2] = initialBlocks
        private var _relativeStakes: Map[Epoch, Map[TaktikosAddress, Ratio]] = initialRelativeStakes

        private var _registrations: Map[Epoch, Map[TaktikosAddress, Box.Values.TaktikosRegistration]] =
          initialRegistrations
        private var _etas: Map[Epoch, Eta] = initialEtas

        def genesis: F[BlockV2] = genesisBlock.pure[F]

        def lookupBlock(id: TypedIdentifier): F[Option[BlockV2]] = _blocks.get(id).pure[F]

        def lookupRelativeStake(epoch: Epoch)(address: TaktikosAddress): F[Option[Ratio]] =
          _relativeStakes.get(epoch).flatMap(_.get(address)).pure[F]

        def lookupRegistration(epoch: Epoch)(address: TaktikosAddress): F[Option[Values.TaktikosRegistration]] =
          _registrations.get(epoch).flatMap(_.get(address)).pure[F]

        def lookupEta(epoch: Epoch): F[Option[Eta]] = _etas.get(epoch).pure[F]

        def canonicalHead: F[BlockV2] =
          if (_blocks.isEmpty) genesis else _blocks.valuesIterator.maxBy(_.headerV2.height).pure[F]

        def append(blockV2: BlockV2): F[Unit] =
          (_blocks += (blockV2.headerV2.id -> blockV2)).pure[F]

        def writeRelativeStakes(epoch: Epoch, relativeStakes: Map[TaktikosAddress, Ratio]): F[Unit] =
          (_relativeStakes += (epoch -> relativeStakes)).pure[F]

        def writeRegistrations(
          epoch:         Epoch,
          registrations: Map[TaktikosAddress, Values.TaktikosRegistration]
        ): F[Unit] =
          (_registrations += (epoch -> registrations)).pure[F]

        def writeEta(epoch: Epoch, eta: Eta): F[Unit] =
          (_etas += (epoch -> eta)).pure[F]
      }
  }
}
