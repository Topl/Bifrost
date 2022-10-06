package co.topl.genusLibrary

import co.topl.genusLibrary.util.Log
import com.typesafe.scalalogging.Logger

import scala.util.{Success, Try}

/**
 * This is the main class of Genus. It captures blocks in an OrientDB database and provides ways to index and query the
 * blocks, transactions and boxes.
 */
class Genus {
  import Genus.logger


}

object Genus {
  private implicit val logger: Logger = Logger(classOf[Genus])

  logger.info("Constructing Genus instance")

  /**
   * This variable refers to the running instance of Genus when it is defined.
   * Call the startup method to define it.
   */
  private val instance = Option.empty[Genus]

  /**
   * This method returns the active instance of Genus. If one does not exist, it is created.
   */
  def getGenus:Try[Genus] = {
    Log.debug("getGenus called: {}") {
      this.synchronized {
        instance.map(Success(_)).getOrElse(Try(new Genus))
      }
    }
  }

}
