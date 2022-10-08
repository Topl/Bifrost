package co.topl.genusLibrary

import com.orientechnologies.orient.server.OServerMain
import com.typesafe.scalalogging.Logger

import java.io.File

/**
 * This is a class to hide the details of interacting with OrientDB.
 */
class OrientDBFacade {
  import OrientDBFacade.logger

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
    ensureGenusDirectoryExists()
  }

  private def ensureGenusDirectoryExists(): Unit = {
    if (!dbDirectory.isDirectory)
      if (dbDirectory.exists)
        throw GenusException(s"${dbDirectory.getAbsolutePath} exists but is not a directory.")
      else if (!dbDirectory.mkdir())
        throw GenusException(s"Failed to create directory ${dbDirectory.getAbsolutePath}")
    else logger.debug("Using existing Genus database directory {}", dbDirectory.getAbsolutePath)
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
}
