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
class OrientDBFacade() {

  import OrientDBFacade._

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
    setupOrientDBEnvironment(new File(orientDbDirectory)).recover(e => throw e)
    new OrientDBFacade
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
      }
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
    |    <handlers>
    |        <handler class="com.orientechnologies.orient.graph.handler.OGraphServerHandler">
    |            <parameters>
    |                <parameter name="enabled" value="true"/>
    |                <parameter name="graph.pool.max" value="50"/>
    |            </parameters>
    |        </handler>
    |        <!-- CLUSTER PLUGIN, TO TURN ON SET THE 'ENABLED' PARAMETER TO 'true' -->
    |        <handler class="com.orientechnologies.orient.server.hazelcast.OHazelcastPlugin">
    |            <parameters>
    |                <!-- <parameter name="nodeName" value="europe1" /> -->
    |                <parameter name="enabled" value="false"/>
    |                <parameter name="configuration.db.default"
    |                           value="${ORIENTDB_HOME}/config/default-distributed-db-config.json"/>
    |                <parameter name="configuration.hazelcast" value="${ORIENTDB_HOME}/config/hazelcast.xml"/>
    |            </parameters>
    |        </handler>
    |        <!-- JMX SERVER, TO TURN ON SET THE 'ENABLED' PARAMETER TO 'true' -->
    |        <handler class="com.orientechnologies.orient.server.handler.OJMXPlugin">
    |            <parameters>
    |                <parameter name="enabled" value="false"/>
    |                <parameter name="profilerManaged" value="true"/>
    |            </parameters>
    |        </handler>
    |        <!-- AUTOMATIC BACKUP, TO TURN ON SET THE 'ENABLED' PARAMETER TO 'true' -->
    |        <handler class="com.orientechnologies.orient.server.handler.OAutomaticBackup">
    |            <parameters>
    |                <parameter name="enabled" value="false"/>
    |                <parameter name="delay" value="4h"/>
    |                <parameter name="target.directory" value="backup"/>
    |                <parameter name="target.fileName" value="${DBNAME}-${DATE:yyyyMMddHHmmss}.zip"/>
    |                <parameter name="compressionLevel" value="9"/>
    |                <parameter name="bufferSize" value="1048576"/>
    |                <!--${DBNAME} AND ${DATE:} VARIABLES ARE SUPPORTED -->
    |                <parameter name="db.include" value=""/>
    |                <!-- DEFAULT: NO ONE, THAT MEANS ALL DATABASES. USE COMMA TO SEPARATE MULTIPLE DATABASE NAMES -->
    |                <parameter name="db.exclude" value=""/>
    |                <!-- USE COMMA TO SEPARATE MULTIPLE DATABASE NAMES -->
    |            </parameters>
    |        </handler>
    |        <!-- SERVER SIDE SCRIPT INTERPRETER. WARNING! THIS CAN BE A SECURITY HOLE:
    |            ENABLE IT ONLY IF CLIENTS ARE TRUCT, TO TURN ON SET THE 'ENABLED' PARAMETER
    |            TO 'true' -->
    |        <handler
    |                class="com.orientechnologies.orient.server.handler.OServerSideScriptInterpreter">
    |            <parameters>
    |                <parameter name="enabled" value="true"/>
    |                <parameter name="allowedLanguages" value="SQL"/>
    |            </parameters>
    |        </handler>
    |        <!-- USE SESSION TOKEN, TO TURN ON SET THE 'ENABLED' PARAMETER TO 'true' -->
    |        <handler class="com.orientechnologies.orient.server.token.OrientTokenHandler">
    |            <parameters>
    |                <parameter name="enabled" value="false"/>
    |                <!-- PRIVATE KEY -->
    |                <parameter name="oAuth2Key" value=""/>
    |                <!-- SESSION LENGTH IN MINUTES, DEFAULT=1 HOUR -->
    |                <parameter name="sessionLength" value="60"/>
    |                <!-- ENCRYPTION ALGORITHM, DEFAULT=HmacSHA256 -->
    |                <parameter name="encryptionAlgorithm" value="HmacSHA256"/>
    |            </parameters>
    |        </handler>
    |    </handlers>
    |    <network>
    |        <sockets>
    |            <socket implementation="com.orientechnologies.orient.server.network.OServerSSLSocketFactory" name="ssl">
    |                <parameters>
    |                    <parameter value="false" name="network.ssl.clientAuth"/>
    |                    <parameter value="config/cert/orientdb.ks" name="network.ssl.keyStore"/>
    |                    <parameter value="password" name="network.ssl.keyStorePassword"/>
    |                    <parameter value="config/cert/orientdb.ks" name="network.ssl.trustStore"/>
    |                    <parameter value="password" name="network.ssl.trustStorePassword"/>
    |                </parameters>
    |            </socket>
    |            <socket implementation="com.orientechnologies.orient.server.network.OServerSSLSocketFactory" name="https">
    |                <parameters>
    |                    <parameter value="false" name="network.ssl.clientAuth"/>
    |                    <parameter value="config/cert/orientdb.ks" name="network.ssl.keyStore"/>
    |                    <parameter value="password" name="network.ssl.keyStorePassword"/>
    |                    <parameter value="config/cert/orientdb.ks" name="network.ssl.trustStore"/>
    |                    <parameter value="password" name="network.ssl.trustStorePassword"/>
    |                </parameters>
    |            </socket>
    |        </sockets>
    |        <protocols>
    |            <!-- Default registered protocol. It reads commands using the HTTP protocol
    |                and write data locally -->
    |            <protocol name="binary"
    |                      implementation="com.orientechnologies.orient.server.network.protocol.binary.ONetworkProtocolBinary"/>
    |            <protocol name="http"
    |                      implementation="com.orientechnologies.orient.server.network.protocol.http.ONetworkProtocolHttpDb"/>
    |        </protocols>
    |        <listeners>
    |            <listener protocol="binary" ip-address="0.0.0.0" port-range="2424-2430" socket="default"/>
    |            <!-- <listener protocol="binary" ip-address="0.0.0.0" port-range="2434-2440" socket="ssl"/> -->
    |            <listener protocol="http" ip-address="0.0.0.0" port-range="2480-2490" socket="default">
    |                <parameters>
    |                    <!-- Connection's custom parameters. If not specified the global configuration
    |                        will be taken -->
    |                    <parameter name="network.http.charset" value="utf-8"/>
    |                    <!-- Define additional HTTP headers to always send as response -->
    |                    <!-- Allow cross-site scripting -->
    |                    <!-- parameter name="network.http.additionalResponseHeaders" value="Access-Control-Allow-Origin:
    |                        *;Access-Control-Allow-Credentials: true" / -->
    |                </parameters>
    |                <commands>
    |                    <command
    |                            pattern="GET|www GET|studio/ GET| GET|*.htm GET|*.html GET|*.xml GET|*.jpeg GET|*.jpg GET|*.png GET|*.gif GET|*.js GET|*.css GET|*.swf GET|*.ico GET|*.txt GET|*.otf GET|*.pjs GET|*.svg GET|*.json GET|*.woff GET|*.ttf GET|*.svgz"
    |                            implementation="com.orientechnologies.orient.server.network.protocol.http.command.get.OServerCommandGetStaticContent">
    |                        <parameters>
    |                            <!-- Don't cache html resources in development mode -->
    |                            <entry name="http.cache:*.htm *.html"
    |                                   value="Cache-Control: no-cache, no-store, max-age=0, must-revalidate\r\nPragma: no-cache"/>
    |                            <!-- Default caching -->
    |                            <entry name="http.cache:default" value="Cache-Control: max-age=120"/>
    |                        </parameters>
    |                    </command>
    |                    <command pattern="GET|gephi/*" implementation="com.orientechnologies.orient.graph.server.command.OServerCommandGetGephi"/>
    |                </commands>
    |            </listener>
    |        </listeners>
    |        <cluster>
    |        </cluster>
    |    </network>
    |    <storages>
    |        <storage name="test" userName="admin" userPassword="admin" path='memory:test' loaded-at-startup="true" db-type="graph" />
    |        <storage name="development" userName="admin" userPassword="admin" path="plocal:/opt/orientdb/databases/development" loaded-at-startup="true" db-type="graph" />
    |    </storages>
    |    <users>
    |        <user resources="*" password="root" name="root"/>
    |    </users>
    |    <properties>
    |        <!-- DATABASE POOL: size min/max -->
    |        <entry name="db.pool.min" value="1"/>
    |        <entry name="db.pool.max" value="50"/>
    |
    |        <!-- PROFILER: configures the profiler as <seconds-for-snapshot>,<archive-snapshot-size>,<summary-size> -->
    |        <entry name="profiler.enabled" value="true"/>
    |        <!-- <entry name="profiler.config" value="30,10,10" /> -->
    |
    |        <!-- LOG: enable/Disable logging. Levels are: finer, fine, finest, info,
    |            warning -->
    |        <entry name="log.console.level" value="info"/>
    |        <entry name="log.file.level" value="fine"/>
    |    </properties>
    |</orient-server>""".stripMargin
  def writeDefaultDbServerConfigFile(file: File): Unit = {
    val outputWriter = new BufferedWriter(new FileWriter(file))
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
