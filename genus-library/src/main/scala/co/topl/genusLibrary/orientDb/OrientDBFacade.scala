package co.topl.genusLibrary.orientDb

import co.topl.genusLibrary.{Genus, GenusException}
import co.topl.typeclasses.Log._
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.tinkerpop.blueprints.impls.orient.{OrientGraphFactory, OrientGraphNoTx}
import com.typesafe.scalalogging.Logger

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.charset.Charset
import scala.annotation.unused
import scala.collection.mutable
import scala.util.{Random, Success, Try}

/**
 * This is a class to hide the details of interacting with OrientDB.
 */
class OrientDBFacade(dir: File, password: String) {
  import OrientDBFacade._
  logger.info("Starting OrientDB with DB server")
  private val dbUserName = "admin"

  logger.debug("creating graph factory")
  private val factory = new OrientGraphFactory(fileToPlocalUrl(dir), dbUserName, password)

  @unused
  private val graphMetadata = initializeDatabase(factory, password)

  private def initializeDatabase(factory: OrientGraphFactory, password: String) = {
    val session: OrientGraphNoTx = factory.getNoTx
    try {
      session.setUseLightweightEdges(true)
      logger.info("Changing password")
      session.command(new OCommandSQL(s"UPDATE OUser SET password='$password' WHERE name='$dbUserName'")).execute()
      logger.info("Configuring Schema")
      new GenusGraphMetadata(session)
    } finally
      session.shutdown()
  }

  private def fileToPlocalUrl(dir: File): String =
    "plocal:" + dir.getAbsolutePath

  /**
   * Shut down the OrientDB server.
   *
   * @return true if the server was running and got shut down
   */
  def shutdown(): Boolean =
    Try {
      factory.close()
    }.logIfFailure("Error while shutting down database").isSuccess
}

object OrientDBFacade {
  implicit private val logger: Logger = Logger(classOf[Genus])

  private val charsetUtf8: Charset = Charset.forName("UTF-8")
  private val passwdLength = 30

  /**
   * Create an instance of OrientDBFacade
   *
   * @return the new instance
   */
  def apply(orientDbDirectory: String = "./genus_db"): OrientDBFacade = {
    val dir = new File(orientDbDirectory)
    setupOrientDBEnvironment(dir)
      .flatMap(password => Success(new OrientDBFacade(dir, password)))
      .recover(excp => throw excp)
      .get
  }

  private[genusLibrary] def ensureDirectoryExists(directory: File): Unit =
    if (!directory.isDirectory)
      if (directory.exists)
        throw GenusException(s"${directory.getAbsolutePath} exists but is not a directory.")
      else if (!directory.mkdir())
        throw GenusException(s"Failed to create directory ${directory.getAbsolutePath}")
      else logger.debug("Using existing directory {}", directory.getAbsolutePath)

  private[genusLibrary] def setupOrientDBEnvironment(dbDirectory: File): Try[String] = {
    ensureDirectoryExists(dbDirectory)
    System.setProperty("ORIENTDB_HOME", dbDirectory.getAbsolutePath)
    rootPassword(passwordFile(dbDirectory))
      .logIfFailure("Failed to read password")
  }

  private[genusLibrary] def passwordFile(dir: File) = new File(dir, "root_pwd")

  /**
   * Read the database root password from a file. If the file does not exist then create the file with a random
   * password.
   *
   * @param pwdFilePath The path of the file that that random password is or will be stored in.
   * @return the database password.
   * @throws java.io.IOException if it needs to write the file but cannot
   */
  private[genusLibrary] def rootPassword(pwdFilePath: File): Try[String] =
    ensurePasswordFileExistsAndOpenIt(pwdFilePath)
      .flatMap { inputStream =>
        try
          readPassword(inputStream)
            .logIfFailure(s"Failed to read password from ${pwdFilePath.getAbsolutePath}")
        finally
          inputStream.close()
      }

  private def ensurePasswordFileExistsAndOpenIt(pwdFilePath: File): Try[FileInputStream] =
    Try(new FileInputStream(pwdFilePath))
      .orElse {
        writePasswordFile(pwdFilePath)
        Success(new FileInputStream(pwdFilePath))
      }
      .logIfFailure(s"Unable to open ${pwdFilePath.getAbsolutePath}")

  private def readPassword(inputStream: FileInputStream) = {
    val buffer: Array[Byte] = Array.ofDim(passwdLength * 2)
    Try {
      val bytesRead = inputStream.read(buffer)
      new String(buffer, 0, bytesRead, charsetUtf8)
    }
  }

  private def writePasswordFile(file: File): Unit = {
    val outputStream = new FileOutputStream(file)
    try {
      val pwdBuilder = new mutable.StringBuilder
      1 to passwdLength foreach { _ =>
        pwdBuilder += Random.nextPrintableChar()
      }
      val bytes = pwdBuilder.toString().replaceAll("'", "").getBytes(charsetUtf8)
      logger.debug("writing {} bytes to password file", bytes.length)
      outputStream.write(bytes)
    } finally
      outputStream.close()
  }
}
