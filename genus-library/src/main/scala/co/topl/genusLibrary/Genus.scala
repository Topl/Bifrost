package co.topl.genusLibrary

import co.topl.genusLibrary.util.Log
import co.topl.genusLibrary.util.Log._
import com.typesafe.scalalogging.Logger

import scala.util.Try

/**
 * This is the main class of Genus. It captures blocks in an OrientDB database and provides ways to index and query the
 * blocks, transactions and boxes.
 */
class Genus {
  import Genus.logger

  private val orientDB = OrientDBFacade()

  private def shutDown(): Try[Unit] = {
    logger.info("Genus shutting down")
    Try(orientDB.shutdown())
  }
}

object Genus {
  private implicit val logger: Logger = Logger(classOf[Genus])

  /**
   * This variable refers to the running instance of Genus.
   */
  private lazy val instance = new Genus

  /**
   * This method returns the active instance of Genus. If one does not exist, it is created.
   */
  def getGenus:Try[Genus] = {
    Log.debug("getGenus called: {}") {
      Try(instance)
        .logIfFailure("Failed to create Genus instance")
    }
  }

  def shutDown(): Try[Unit] = {
    logger.info("shutDown() called")
    instance.shutDown()
  }
}
