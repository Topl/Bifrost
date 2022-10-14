package co.topl.genusLibrary

import com.orientechnologies.orient.server.OServerMain
import com.typesafe.scalalogging.Logger

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.charset.Charset
import scala.util.{Random, Success, Try}

import util.Log._

/**
 * This is a class to hide the details of interacting with OrientDB.
 */
class OrientDBFacade {
  import OrientDBFacade._

  private val charsetUtf8: Charset = Charset.forName("UTF-8")
  val dbDirectory: File = new File("genus_db")
  val pwdFilePath = new File(dbDirectory, "root_pwd")

  setupOrientDBEnvironment().recover( e => throw e )
  logger.info("Starting OrientDB")
  private val server = OServerMain.create(true) // true argument request shutdown of server on exit.
  server.startup() // Use the default OrientDB server configuration
  server.activate()

  /**
   * Shut down the OrientDB server.
   *
   * @return true if the server was running and got shut down
   */
  def shutdown(): Boolean =
    server.shutdown()

  private def setupOrientDBEnvironment(): Try[Unit] = {
    ensureDirectoryExists(dbDirectory)
    System.setProperty("ORIENTDB_HOME", dbDirectory.getAbsolutePath)
    rootPassword
      .logIfFailure("Failed to read password")
      .map(password => System.setProperty("ORIENTDB_ROOT_PASSWORD", password))
  }

  private val passwdLength = 22

  /**
   * Read the database root password from a file. If the file does not exist then create the file with a random
   * password.
   *
   * @return the database password.
   * @throws java.io.IOException if it needs to write the file but cannot
   */
  private[genusLibrary] def rootPassword: Try[String] =
    ensurePasswordFileExistsAndOpenIt
      .flatMap { inputStream =>
        try
          readPassword(inputStream)
        finally
          inputStream.close()
      }

  private def ensurePasswordFileExistsAndOpenIt = {
    Try(new FileInputStream(pwdFilePath))
      .orElse {
        writePasswordFile()
        Success(new FileInputStream(pwdFilePath))
      }
      .logIfFailure(s"Unable to open ${pwdFilePath.getAbsolutePath}")
  }

  private def readPassword(inputStream: FileInputStream) = {
    Try {
      val buffer: Array[Byte] = Array(passwdLength * 2)
      val bytesRead = inputStream.read(buffer)
      new String(buffer, 0, bytesRead)
    }.logIfFailure(s"Failed to read ${pwdFilePath.getAbsolutePath}")
  }

  private def writePasswordFile(): Unit = {
    val outputStream = new FileOutputStream(pwdFilePath)
    try
      outputStream.write(Random.nextString(passwdLength).getBytes(charsetUtf8))
    finally
      outputStream.close()
  }
}

object OrientDBFacade {
  implicit private val logger: Logger = Logger(classOf[Genus])

  /**
   * Create an instance of OrientDBFacade
   *
   * @return the new instance
   */
  def apply(): OrientDBFacade = new OrientDBFacade()

  private[genusLibrary] def ensureDirectoryExists(directory: File): Unit =
    if (!directory.isDirectory)
      if (directory.exists)
        throw GenusException(s"${directory.getAbsolutePath} exists but is not a directory.")
      else if (!directory.mkdir())
        throw GenusException(s"Failed to create directory ${directory.getAbsolutePath}")
      else logger.debug("Using existing directory {}", directory.getAbsolutePath)
}
