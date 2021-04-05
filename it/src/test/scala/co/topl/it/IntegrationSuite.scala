package co.topl.it

import akka.actor.{ActorSystem, Scheduler}
import co.topl.it.util.{BifrostDockerNode, DockerSupport}
import co.topl.utils.Logging
import com.spotify.docker.client.DefaultDockerClient
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, EitherValues, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

trait IntegrationSuite extends BeforeAndAfterAll with Logging with ScalaFutures with EitherValues {
  self: Suite =>

  implicit val system: ActorSystem = ActorSystem("TestSuite")

  implicit val ec: ExecutionContext = system.dispatcher

  implicit val scheduler: Scheduler = system.scheduler

  implicit val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  implicit val dockerSupport: DockerSupport = new DockerSupport(dockerClient)

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(2.seconds)

  override def beforeAll(): Unit =
    log.debug("Starting integration tests")

  override def afterAll(): Unit = {
    dockerSupport.close()
    dockerClient.close()
    Await.result(system.terminate(), 10.seconds)
  }

  /** The genesis block contains pre-loaded addresses, one for each of our test nodes.  Assign
    * each test node to a single address by locking each node out of all-but-one address, where the assigned
    * address is determined by the node's index in the given list.
    */
  protected def assignForgingAddress(nodes: List[BifrostDockerNode]): Unit = {
    val allAddresses: Map[String, List[String]] =
      Future
        .traverse(nodes)(node => node.Admin.listOpenKeyfiles().map(node.containerId -> _.value))
        .futureValue(Timeout(10.seconds))
        .toMap

    require(allAddresses.values.toSet.size == 1, "Nodes do not contain the same addresses")

    val addressList = allAddresses.head._2

    nodes.zipWithIndex.foreach { case (node, index) =>
      addressList.zipWithIndex.foreach {
        case (address, addressIndex) if addressIndex != index =>
          node.Admin.lockKeyfile(address).futureValue.value
        case (address, _) =>
          node.Admin.updateRewardsAddress(address).futureValue.value
      }
    }
  }
}
