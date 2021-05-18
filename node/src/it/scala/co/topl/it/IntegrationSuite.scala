package co.topl.it

import akka.actor.{ActorSystem, Scheduler}
import co.topl.attestation.{Address, Evidence, PublicKeyPropositionCurve25519}
import co.topl.it.util.{BifrostDockerNode, DockerSupport}
import co.topl.rpc.{ToplRpc, ToplRpcClientCodecs}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Logging, NetworkType}
import com.spotify.docker.client.DefaultDockerClient
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, EitherValues, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

trait IntegrationSuite
    extends BeforeAndAfterAll
    with Logging
    with ScalaFutures
    with EitherValues
    with ToplRpcClientCodecs {
  self: Suite =>

  implicit val system: ActorSystem = ActorSystem("TestSuite")

  implicit val ec: ExecutionContext = system.dispatcher

  implicit val scheduler: Scheduler = system.scheduler

  implicit val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  implicit val dockerSupport: DockerSupport = new DockerSupport(dockerClient)

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(2.seconds)

  implicit val networkPrefix: NetworkPrefix = NetworkType.PrivateTestnet.netPrefix

  implicit def futureAwaiter[T]: Future[T] => T = _.futureValue

  override def beforeAll(): Unit =
    log.debug("Starting integration tests")

  override def afterAll(): Unit = {
    dockerSupport.close()
    dockerClient.close()
    Await.result(system.terminate(), 10.seconds)
  }

  /**
   * The genesis block contains pre-loaded addresses, one for each of our test nodes.  Assign
   * each test node to a single address by locking each node out of all-but-one address, where the assigned
   * address is determined by the node's index in the given list.
   */
  protected def assignForgingAddresses(nodes: List[BifrostDockerNode]): Unit = {
    val allAddresses: Map[String, Set[Address]] =
      nodes
        .map(node =>
          node.containerId -> node
            .run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params())
            .value
            .unlocked
        )
        .toMap

    require(allAddresses.values.toSet.size == 1, "Nodes do not contain the same addresses")

    val addressList = allAddresses.head._2.toList

    nodes.zipWithIndex.foreach { case (node, index) =>
      addressList.zipWithIndex.foreach {
        case (address, addressIndex) if addressIndex != index =>
          node.run(ToplRpc.Admin.LockKeyfile.rpc)(ToplRpc.Admin.LockKeyfile.Params(address)).value
        case (address, _) =>
          node.run(ToplRpc.Admin.UpdateRewardsAddress.rpc)(ToplRpc.Admin.UpdateRewardsAddress.Params(address)).value
      }
    }
  }

  /**
   * Assigns a specific rewards address to a node, and locks out all other keys from the node's ring.  The given address
   * should already exist in the forger's keyring
   */
  protected def assignForgingAddress(node: BifrostDockerNode, address: Address): Unit = {
    val unlockedAddresses = node
      .run(ToplRpc.Admin.ListOpenKeyfiles.rpc)(ToplRpc.Admin.ListOpenKeyfiles.Params())
      .value
      .unlocked

    unlockedAddresses.foreach {
      case `address` =>
        node.run(ToplRpc.Admin.UpdateRewardsAddress.rpc)(ToplRpc.Admin.UpdateRewardsAddress.Params(address)).value
      case a =>
        node.run(ToplRpc.Admin.LockKeyfile.rpc)(ToplRpc.Admin.LockKeyfile.Params(a)).value
    }

  }

  protected def wrapNode[T](node: BifrostDockerNode)(f: BifrostDockerNode => T): T =
    f(node)

  protected def addressFromBytes(b: Array[Byte]): Address =
    Address(Evidence(PublicKeyPropositionCurve25519.typePrefix, Evidence.EvidenceContent @@ b))
}
