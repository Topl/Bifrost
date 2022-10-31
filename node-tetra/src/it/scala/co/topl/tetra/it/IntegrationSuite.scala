package co.topl.tetra.it

import co.topl.tetra.it.util.DockerSupport
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Logging, NetworkType}
import com.spotify.docker.client.DefaultDockerClient
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, EitherValues, Suite}

import scala.concurrent.duration._

trait IntegrationSuite extends BeforeAndAfterAll with Logging with ScalaFutures with EitherValues {
  self: Suite =>

  implicit val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  implicit val dockerSupport: DockerSupport = new DockerSupport(dockerClient)

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(2.seconds)

  implicit val networkPrefix: NetworkPrefix = NetworkType.PrivateTestnet.netPrefix

  override def beforeAll(): Unit =
    log.debug("Starting integration test")

  override def afterAll(): Unit = {
    dockerSupport.close()
    dockerClient.close()
  }

}
