package co.topl.genusLibrary

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
  private val logger = Logger(classOf[Genus])

  /**
   * This variable refers to the running instance of Genus when it is defined.
   * Call the startup method to define it.
   */
  val instance = Option.empty[Genus]

  /**
   * This method must be called to start Genus before any other operations are allowed
   */
  def startUp:Try[Genus] = {
    this.synchronized {
      Success(instance.get)
    }
  }

}
