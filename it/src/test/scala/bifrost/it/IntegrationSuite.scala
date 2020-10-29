package bifrost.it

import java.util.concurrent.Executors

import bifrost.it.util.Docker
import bifrost.utils.Logging
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.ExecutionContext
import scala.util.Random

trait IntegrationSuite extends IntegrationConstants with BeforeAndAfterAll with Logging { this: Suite =>

  implicit val defaultExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  protected val localDataDir: String = s"/tmp/bifrost/it-${Random.nextInt(Int.MaxValue)}"

  protected val docker: Docker = new Docker()

  override def beforeAll(): Unit = {
    log.debug("Starting integration tests")
  }

  override def afterAll(): Unit = docker.close()
}
