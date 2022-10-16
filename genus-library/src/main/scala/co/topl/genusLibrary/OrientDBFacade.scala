package co.topl.genusLibrary

import com.orientechnologies.orient.server.OServerMain
import com.typesafe.scalalogging.Logger

import java.io.{BufferedWriter, File, FileInputStream, FileOutputStream, FileWriter}
import java.nio.charset.Charset
import scala.util.{Random, Success, Try}
import util.Log._

import scala.collection.mutable

/**
 * This is a class to hide the details of interacting with OrientDB.
 */
class OrientDBFacade(dir: File) {

  import OrientDBFacade._

  private val configFile = serverConfigFile(dir)
  logger.info("Starting OrientDB with DB server config file {}", configFile.getAbsolutePath)
  private val server = OServerMain.create(true) // true argument request shutdown of server on exit.
  server.startup(configFile)
  server.activate()

  /**
   * Shut down the OrientDB server.
   *
   * @return true if the server was running and got shut down
   */
  def shutdown(): Boolean =
    server.shutdown()
}

object OrientDBFacade {
  implicit private val logger: Logger = Logger(classOf[Genus])

  private val charsetUtf8: Charset = Charset.forName("UTF-8")
  private val passwdLength = 22
  private val dbConfigFileName = "orientdb-server-config.xml"

  /**
   * Create an instance of OrientDBFacade
   *
   * @return the new instance
   */
  def apply(orientDbDirectory: String = "./genus_db"): OrientDBFacade = {
    val dir = new File(orientDbDirectory)
    setupOrientDBEnvironment(dir).recover(e => throw e)
    new OrientDBFacade(dir)
  }

  private[genusLibrary] def ensureDirectoryExists(directory: File): Unit =
    if (!directory.isDirectory)
      if (directory.exists)
        throw GenusException(s"${directory.getAbsolutePath} exists but is not a directory.")
      else if (!directory.mkdir())
        throw GenusException(s"Failed to create directory ${directory.getAbsolutePath}")
      else logger.debug("Using existing directory {}", directory.getAbsolutePath)

  private[genusLibrary] def setupOrientDBEnvironment(dbDirectory: File): Try[Unit] = {
    ensureDirectoryExists(dbDirectory)
    System.setProperty("ORIENTDB_HOME", dbDirectory.getAbsolutePath)
    rootPassword(passwordFile(dbDirectory))
      .logIfFailure("Failed to read password")
      .map(password => System.setProperty("ORIENTDB_ROOT_PASSWORD", password))
      .flatMap(_ => ensureDbServerConfigFileExists(dbDirectory))
  }

  def ensureDbServerConfigFileExists(dbDirectory: File): Try[Unit] = {
    Try {
      val file = serverConfigFile(dbDirectory)
      if (!file.isFile) {
        val dir = configDirectory(dbDirectory)
        ensureDirectoryExists(dir)
        writeDefaultDbServerConfigFile(file)
      } else
        logger.info("Found existing DB server config file {}", file.getAbsolutePath)
    }.logIfFailure("Error accessing or creating DB server configuration file")
  }

  private val defaultDbServerConfig =
  """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    |<!--
    |   ~ /*
    |   ~  *  Copyright 2014 Orient Technologies LTD (info(at)orientechnologies.com)
    |   ~  *
    |   ~  *  Licensed under the Apache License, Version 2.0 (the "License");
    |   ~  *  you may not use this file except in compliance with the License.
    |   ~  *  You may obtain a copy of the License at
    |   ~  *
    |   ~  *       http://www.apache.org/licenses/LICENSE-2.0
    |   ~  *
    |   ~  *  Unless required by applicable law or agreed to in writing, software
    |   ~  *  distributed under the License is distributed on an "AS IS" BASIS,
    |   ~  *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    |   ~  *  See the License for the specific language governing permissions and
    |   ~  *  limitations under the License.
    |   ~  *
    |   ~  * For more information: http://www.orientechnologies.com
    |   ~  */
    |   -->
    |
    |<orient-server>
    |    <network>
    |        <protocols>
    |            <!-- Default registered protocol. It reads commands using the HTTP protocol
    |                and write data locally -->
    |            <protocol name="binary"
    |                      implementation="com.orientechnologies.orient.server.network.protocol.binary.ONetworkProtocolBinary"/>
    |            <protocol name="http"
    |                      implementation="com.orientechnologies.orient.server.network.protocol.http.ONetworkProtocolHttpDb"/>
    |        </protocols>
    |        <listeners>
    |            <listener protocol="binary" ip-address="0.0.0.0" port-range="2424-2430" />
    |            <listener protocol="http" ip-address="0.0.0.0" port-range="2480-2490" />
    |        </listeners>
    |    </network>
    |    <users>
    |        <user resources="*" password="root" name="root"/>
    |    </users>
    |    <properties>
    |        <entry name="server.cache.staticResources" value="false"/>
    |        <!-- LOG: enable/Disable logging. Levels are: finer, fine, finest, info,
    |            warning -->
    |        <entry name="log.console.level" value="info"/>
    |        <entry name="log.file.level" value="fine"/>
    |        <entry name="plugin.dynamic" value="false"/>
    |    </properties>
    |</orient-server>""".stripMargin
  def writeDefaultDbServerConfigFile(file: File): Unit = {
    val outputWriter = new BufferedWriter(new FileWriter(file, charsetUtf8))
    try {
      outputWriter.write(defaultDbServerConfig)
    } finally outputWriter.close()
  }

  private def serverConfigFile(dbDirectory: File): File = new File(configDirectory(dbDirectory), dbConfigFileName)

  private def configDirectory(dbDirectory: File): File = new File(dbDirectory, "config")

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
      val bytes = pwdBuilder.toString().getBytes(charsetUtf8)
      logger.debug("writing {} bytes to password file", bytes.length)
      outputStream.write(bytes)
    } finally
      outputStream.close()
  }
}
