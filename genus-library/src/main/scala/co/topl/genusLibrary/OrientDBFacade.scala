package co.topl.genusLibrary

import com.orientechnologies.orient.server.OServerMain
import com.typesafe.scalalogging.Logger

import java.io.File

/**
 * This is a class to hide the details of interacting with OrientDB.
 */
class OrientDBFacade {
  import OrientDBFacade._

  var dbDirectory = new File("genus")

  setupOrientDBEnvironment()
  logger.info("Starting OrientDB")
  private var server = OServerMain.create(true)  // true argument request shutdown of server on exit.

  /**
   * Shut down the OrientDB server.
   *
   * @return true if the server was running and got shut down
   */
  def shutdown(): Boolean = {
    server.shutdown()
  }

  private def setupOrientDBEnvironment(): Unit = {
    ensureDirectoryExists(dbDirectory)
  }
}

object OrientDBFacade {
  private implicit val logger: Logger = Logger(classOf[Genus])

  /**
   * Create an instance of OrientDBFacade
   *
   * @return the new instance
   */
  def apply(): OrientDBFacade = new OrientDBFacade()

  private[genusLibrary] def ensureDirectoryExists(directory: File): Unit = {
    if (!directory.isDirectory)
      if (directory.exists)
        throw GenusException(s"${directory.getAbsolutePath} exists but is not a directory.")
      else if (!directory.mkdir())
        throw GenusException(s"Failed to create directory ${directory.getAbsolutePath}")
      else logger.debug("Using existing directory {}", directory.getAbsolutePath)
  }
}
