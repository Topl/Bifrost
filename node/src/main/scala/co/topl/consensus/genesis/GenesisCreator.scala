package co.topl.consensus.genesis

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import co.topl.consensus.ConsensusVariables.ConsensusParamsUpdate
import co.topl.consensus.Forger.ChainParams
import co.topl.consensus.KeyManager.StartupKeyView
import co.topl.consensus.{ConsensusVariablesHolder, NxtLeaderElection}
import co.topl.modifier.block.Block
import co.topl.settings.AppSettings
import co.topl.settings.GenesisStrategy.{FromBlockJson, Generated}
import co.topl.utils.NetworkType
import co.topl.utils.NetworkType.{HelTestnet, Mainnet, NetworkPrefix, PrivateTestnet, ValhallaTestnet}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object GenesisCreator {

  def genesisBlock(
    settings:                    AppSettings,
    networkType:                 NetworkType,
    fetchStartupKeyView:         () => Future[StartupKeyView],
    consensusVariablesInterface: ConsensusVariablesHolder
  )(implicit
    nxtLeaderElection: NxtLeaderElection,
    system:            ActorSystem[_],
    ec:                ExecutionContext
  ): Future[Block] = {
    implicit val networkPrefix: NetworkPrefix = networkType.netPrefix

    def initializeFromChainParamsAndGetBlock(block: Try[(Block, ChainParams)]): Future[Block] = {

      import scala.concurrent.duration._
      implicit val timeout: Timeout = Timeout(10.seconds)

      Future.fromTry(block).flatMap { case (block: Block, ChainParams(totalStake, initDifficulty)) =>
        consensusVariablesInterface
          .update(block.id, ConsensusParamsUpdate(Some(totalStake), Some(initDifficulty), Some(0L), Some(0L)))
          .valueOrF(e => Future.failed(e.reason))
          .map(_ => block)
      }
    }

    def generateGenesisBlock: Future[Block] =
      settings.forging.genesis.flatMap(_.generated) match {
        case Some(genesisSettings) =>
          fetchStartupKeyView()
            .map(view => GeneratedGenesis(view.addresses, genesisSettings).getGenesisBlock)
            .flatMap(r => initializeFromChainParamsAndGetBlock(r))
        case None =>
          Future.failed(new IllegalArgumentException("Failed to read generated genesis settings"))
      }

    if (settings.forging.genesis.flatMap(_.genesisStrategy).isEmpty) {
      networkType match {
        case Mainnet         => initializeFromChainParamsAndGetBlock(ToplnetGenesis.getGenesisBlock)
        case ValhallaTestnet => initializeFromChainParamsAndGetBlock(ValhallaGenesis.getGenesisBlock)
        case HelTestnet      => initializeFromChainParamsAndGetBlock(HelGenesis.getGenesisBlock)
        case PrivateTestnet  => generateGenesisBlock
      }
    } else {
      settings.forging.genesis.flatMap(_.genesisStrategy) match {
        case Some(Generated) =>
          generateGenesisBlock
        case Some(FromBlockJson) =>
          settings.forging.genesis.flatMap(_.fromBlockJson) match {
            case Some(genesisSettings) =>
              initializeFromChainParamsAndGetBlock(
                GenesisFromBlockJson(genesisSettings, networkType).getGenesisBlock
              )
            case None =>
              Future.failed(new IllegalArgumentException("Failed to read genesis from block Json settings"))
          }
        case None =>
          Future.failed(
            new IllegalArgumentException(s"Undefined genesis strategy $settings.forging.genesis.head.genesisStrategy")
          )
      }
    }
  }
}
