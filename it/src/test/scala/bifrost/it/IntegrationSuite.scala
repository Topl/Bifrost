package bifrost.it

import bifrost.it.util.Docker
import bifrost.utils.Logging
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.util.Random

trait IntegrationSuite extends BeforeAndAfterAll with Logging { this: Suite =>

  protected val localDataDir: String = s"/tmp/bifrost/it-${Random.nextInt(Int.MaxValue)}"

  protected val docker: Docker = new Docker()

  override def beforeAll(): Unit = {
    log.debug("Starting integration tests")
  }

  override def afterAll(): Unit = docker.close()
}
