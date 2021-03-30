package co.topl.it

import co.topl.it.util._
import co.topl.utils.Int128, Int128._
import com.typesafe.config.ConfigFactory
import io.circe.syntax._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Inspectors}

import scala.concurrent.duration._

class BootstrapFromGenesisTest
    extends AnyFreeSpec
    with Matchers
    with IntegrationSuite
    with ScalaFutures
    with EitherValues
    with Inspectors {

  val forgeDuration: FiniteDuration = 30.seconds
  val newNodeForgeDuration: FiniteDuration = 10.seconds
  val seed: String = "BootstrapFromGenesisTest" + System.currentTimeMillis()

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(2.seconds)

  "A new node can sync its genesis block with an old node" in {

    val nodeNames = List("oldNode", "newNode")

    val config =
      ConfigFactory.parseString(
        raw"""bifrost.network.knownPeers = ${nodeNames.map(n => s"$n:${BifrostDockerNode.NetworkPort}").asJson}
             |bifrost.rpcApi.namespaceSelector.debug = true
             |bifrost.forging.privateTestnet.numTestnetAccts = 2
             |bifrost.forging.privateTestnet.genesisSeed = "$seed"
             |""".stripMargin
      )

    logger.info("Starting oldNode")

    val oldNode = dockerSupport.createNode("oldNode", seed)
    oldNode.reconfigure(config)
    oldNode.start()

    oldNode.waitForStartup().futureValue(Timeout(60.seconds)).value

    val keyFiles =
      oldNode.Admin.listOpenKeyfiles().futureValue.value

    keyFiles should have size 2

    val List(oldKeyFile, newKeyFile) = keyFiles

    oldNode.Admin.lockKeyfile(newKeyFile).futureValue.value

    oldNode.Admin.startForging().futureValue.value

    logger.info(s"Allowing oldNode to forge for $forgeDuration")

    Thread.sleep(forgeDuration.toMillis)

    logger.info("Starting newNode")

    val newNode = dockerSupport.createNode("newNode", seed)
    newNode.reconfigure(config)
    newNode.start()

    newNode.waitForStartup().futureValue(Timeout(60.seconds)).value

    newNode.Admin.lockKeyfile(oldKeyFile).futureValue.value

    newNode.Admin.startForging().futureValue.value

    logger.info(s"Allowing newNode to sync and forge for $newNodeForgeDuration")

    Thread.sleep(newNodeForgeDuration.toMillis)

    val oldNodeHeight: Int128 =
      oldNode.Topl.head().futureValue.value.height
    logger.info(s"Old node height=$oldNodeHeight")

    val newNodeHeight: Int128 =
      newNode.Topl.head().futureValue.value.height
    logger.info(s"New node height=$newNodeHeight")

    oldNodeHeight shouldBe newNodeHeight +- 1
    oldNodeHeight should be > Int128(1L)
  }

}
