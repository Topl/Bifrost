package co.topl.genusLibrary

import co.topl.genusLibrary.util.Log
import com.typesafe.scalalogging.Logger

import scala.reflect.ClassTag.Nothing
import scala.runtime.Nothing$
import scala.util.{Failure, Success, Try}

/**
 * This is the main class of Genus. It captures blocks in an OrientDB database and provides ways to index and query the
 * blocks, transactions and boxes.
 */
class Genus {
  import Genus.logger

  private def shutDown(): Try[Unit] = {
    logger.info("Genus shutting down")
    Success(())
  }
}

object Genus {
  private implicit val logger: Logger = Logger(classOf[Genus])

  logger.info("Constructing Genus instance")

  /**
   * This variable refers to the running instance of Genus when it is defined.
   * Call the startup method to define it.
   */
  private var instance = Option.empty[Genus]

  private def ensureActive[T](thunk: => T): Try[T] = {
    instance.map( _ => Success(thunk)).getOrElse(Failure( GenusException("")))
  }

  /**
   * This method returns the active instance of Genus. If one does not exist, it is created.
   */
  def getGenus:Try[Genus] = {
    Log.debug("getGenus called: {}") {
      this.synchronized {
        instance.map(Success(_)).getOrElse(Try{
          instance = Some(new Genus)
          instance.get
        })
      }
    }
  }

  def shutDown(): Try[Unit] = {
    Log.info("shutDown() called") {
      this.synchronized {
        ensureActive {
          instance.get.shutDown()
          instance = None
        }
      }
    }
  }
}
