package co.topl.it

import co.topl.it.util._
import co.topl.rpc.ToplRpc
import co.topl.utils.Int128
import co.topl.utils.Int128._
import com.typesafe.config.ConfigFactory
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class BootstrapFromGenesisTest
    extends AnyFreeSpec
    with Matchers
    with IntegrationSuite
    with ScalaFutures
    with Inspectors {

  val initialForgeTarget: Int128 = 1500
  val newNodeForgeDuration: FiniteDuration = 10.seconds
  val targetBlockTime: FiniteDuration = 250.milli
  val syncWindow: FiniteDuration = 60.seconds
  val seed: String = "BootstrapFromGenesisTest" + System.currentTimeMillis()

  "A new node can sync its genesis block with an old node" in {

    val config =
      ConfigFactory.parseString(
        raw"""bifrost.network.knownPeers = []
             |bifrost.rpcApi.namespaceSelector.debug = true
             |bifrost.forging.forgeOnStartup = false
             |bifrost.forging.blockGenerationDelay = ${targetBlockTime / 2}
             |bifrost.forging.addressGenerationSettings.numberOfAddresses = 2
             |bifrost.forging.addressGenerationSettings.strategy = fromSeed
             |bifrost.forging.addressGenerationSettings.addressSeedOpt = "$seed"
             |bifrost.forging.protocolVersions = [
             |      {
             |        minAppVersion {value = 1.0.0}
             |        startBlock = 0
             |        blockVersion = 1
             |        value {
             |          targetBlockTime = $targetBlockTime
             |          numTxPerBlock = 100
             |          inflationRate = 0
             |          lookBackDepth = 3
             |        }
             |      }
             |    ]
             |bifrost.network.syncInterval = 50ms
             |bifrost.network.syncIntervalStable = 500s
             |""".stripMargin
      )

    logger.info("Starting oldNode")

    val oldNode: BifrostDockerNode = dockerSupport.createNode("oldNode", seed)
    oldNode.reconfigure(config)
    oldNode.start()

    oldNode.waitForStartup().futureValue(Timeout(60.seconds)).value

    val keyFiles =
      oldNode.run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params()).value.unlocked

    keyFiles should have size 2

    val List(oldKeyFile, newKeyFile) = keyFiles.toList

    oldNode.run(ToplRpc.Admin.LockKeyfile.rpc)(ToplRpc.Admin.LockKeyfile.Params(newKeyFile)).value

    oldNode.run(ToplRpc.Admin.StartForging.rpc)(ToplRpc.Admin.StartForging.Params()).value

    logger.info(s"Allowing oldNode to forge $initialForgeTarget blocks.  This may take a while.")

    oldNode.pollUntilHeight(initialForgeTarget).futureValue(Timeout(45.minutes)).value

    logger.info("Starting newNode")

    val newNode = dockerSupport.createNode("newNode", seed)

    val newNodeConfig =
      ConfigFactory
        .parseString(
          raw"""bifrost.network.knownPeers = ["oldNode:${BifrostDockerNode.NetworkPort}"]
               |""".stripMargin
        )
        .withFallback(config)

    newNode.reconfigure(newNodeConfig)
    newNode.start()

    newNode.waitForStartup().futureValue(Timeout(60.seconds)).value

    Thread.sleep(5.seconds.toMillis)

    newNode.run(ToplRpc.Admin.LockKeyfile.rpc)(ToplRpc.Admin.LockKeyfile.Params(oldKeyFile)).value

    newNode.run(ToplRpc.Admin.StartForging.rpc)(ToplRpc.Admin.StartForging.Params()).value

    logger.info(s"Allowing newNode to sync and forge for $newNodeForgeDuration")

    Thread.sleep(newNodeForgeDuration.toMillis)

    logger.info("Stopping forging on both nodes")

    oldNode.run(ToplRpc.Admin.StopForging.rpc)(ToplRpc.Admin.StopForging.Params()).value
    newNode.run(ToplRpc.Admin.StopForging.rpc)(ToplRpc.Admin.StopForging.Params()).value

    logger.info(s"Waiting $syncWindow for the nodes to sync")

    Thread.sleep(syncWindow.toMillis)

    val oldNodeHead =
      oldNode.run(ToplRpc.NodeView.Head.rpc)(ToplRpc.NodeView.Head.Params()).value
    logger.info(s"Old node height=${oldNodeHead.height} bestBlockId=${oldNodeHead.bestBlockId}")

    val newNodeHead =
      newNode.run(ToplRpc.NodeView.Head.rpc)(ToplRpc.NodeView.Head.Params()).value
    logger.info(s"New node height=${newNodeHead.height} bestBlockId=${newNodeHead.bestBlockId}")

    oldNodeHead.bestBlock shouldBe newNodeHead.bestBlock
    oldNodeHead.height should be >= initialForgeTarget
  }

}
