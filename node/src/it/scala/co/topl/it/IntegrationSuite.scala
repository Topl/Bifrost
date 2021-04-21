package co.topl.it

import akka.actor.ActorSystem
import co.topl.it.util.DockerSupport
import co.topl.utils.Logging
import com.spotify.docker.client.DefaultDockerClient
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.Await
import scala.concurrent.duration._

trait IntegrationSuite extends BeforeAndAfterAll with Logging { this: Suite =>

  implicit val system: ActorSystem = ActorSystem("TestSuite")

  implicit val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  implicit val dockerSupport: DockerSupport = new DockerSupport(dockerClient)

  override def beforeAll(): Unit =
    log.debug("Starting integration tests")

  override def afterAll(): Unit = {
    dockerSupport.close()
    dockerClient.close()
    Await.result(system.terminate(), 10.seconds)
  }
}
