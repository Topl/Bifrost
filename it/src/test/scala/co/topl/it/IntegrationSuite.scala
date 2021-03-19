package co.topl.it

import akka.actor.ActorSystem
import co.topl.it.util.DockerSupport
import co.topl.utils.Logging
import com.spotify.docker.client.DefaultDockerClient
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.util.Random

trait IntegrationSuite extends IntegrationConstants with BeforeAndAfterAll with Logging { this: Suite =>

  implicit val system: ActorSystem = ActorSystem("TestSuite")

  protected val localDataDir: String = s"/tmp/bifrost/it-${Random.nextInt(Int.MaxValue)}"

  implicit val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  protected implicit val dockerSupport: DockerSupport = new DockerSupport(dockerClient)

  override def beforeAll(): Unit =
    log.debug("Starting integration tests")

  override def afterAll(): Unit = {
    dockerSupport.close()
    dockerClient.close()
  }
}
