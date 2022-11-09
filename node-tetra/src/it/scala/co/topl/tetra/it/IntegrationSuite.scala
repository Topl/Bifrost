package co.topl.tetra.it

import co.topl.tetra.it.util.DockerSupport
import com.spotify.docker.client.DefaultDockerClient
import com.typesafe.scalalogging.{Logger, StrictLogging}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, EitherValues, Suite}

import scala.concurrent.duration._

trait IntegrationSuite extends BeforeAndAfterAll with ScalaFutures with EitherValues with StrictLogging {
  self: Suite =>

  protected def log: Logger = logger

  implicit val dockerClient: DefaultDockerClient = DefaultDockerClient.fromEnv().build()

  val dockerSupport: DockerSupport = new DockerSupport(dockerClient)

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(2.seconds)

  override def beforeAll(): Unit =
    log.debug("Starting integration test")

  override def afterAll(): Unit = {
    dockerSupport.close()
    dockerClient.close()
  }

}
