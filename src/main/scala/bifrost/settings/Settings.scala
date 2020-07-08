package bifrost.settings

import java.io.File
import java.net.InetSocketAddress

import bifrost.modifier.box.proposition.Constants25519._
import bifrost.utils.Logging
import io.circe.Json
import io.circe.parser.parse
import scorex.crypto.encode.Base58

import scala.concurrent.duration._
import scala.util.{Random, Try}

/**
  * Settings
  */
trait Settings extends Logging {

  def settingsFromFile(filename: String): Map[String, Json] = Try {
    val jsonString = scala.io.Source.fromFile(filename).mkString
    parse(jsonString).right.get
  }.recoverWith { case t =>
    Try {
      val jsonString = scala.io.Source.fromURL(getClass.getResource(s"/$filename")).mkString
      parse(jsonString).right.get
    }
  }.toOption.flatMap(_.asObject).map(_.toMap).getOrElse {
    log.error(s"Unable to read $filename or not a JSON map there, closing")
    //catch error?
    System.exit(10)
    Map()
  }

  def settingsJSON: Map[String, Json]

  private def directoryEnsuring(dirPath: String): Boolean = {
    val f = new java.io.File(dirPath)
    f.mkdirs()
    f.exists()
  }

  private def folderOpt(settingName: String): Option[String] = {
    val res = settingsJSON.get(settingName).flatMap(_.asString)
    res.foreach(folder => new File(folder).mkdirs())
    require(res.isEmpty || new File(res.get).exists())
    res
  }

  lazy val dataDirOpt: Option[String] = folderOpt("dataDir")
  lazy val logDirOpt: Option[String] = folderOpt("logDir")
  lazy val pbrDirOpt: Option[String] = folderOpt("pbrDir")
  lazy val tbrDirOpt: Option[String] = folderOpt("tbrDir")

  lazy val walletDirOpt: Option[String] = settingsJSON.get("walletDir").flatMap(_.asString)
    .ensuring(pathOpt => pathOpt.forall(directoryEnsuring))

  lazy val nodeKeys: Option[Set[String]] = settingsJSON.get("nodeKeys")
    .flatMap(_.asArray).map(_.flatMap(_.asString)).map(_.toSet)

  //p2p
  lazy val DefaultPort = 9084
  lazy val DefaultHandshakeTimeout = 5000

  lazy val p2pSettings: Map[String, Json] = settingsJSON("p2p").asObject.get.toMap

  lazy val nodeNonce: Long = (Random.nextInt(1000) + 1000) * Random.nextInt(1000) + Random.nextInt(1000)

  lazy val addedMaxDelay: Option[Int] = p2pSettings.get("addedMaxDelay").flatMap(_.asNumber).flatMap(_.toInt).flatMap { i =>
    if (i == 0) None else Some(i)
  }

  lazy val nodeName: String = p2pSettings.get("name").flatMap(_.asString)
    .getOrElse(Random.nextPrintableChar().toString + nodeNonce)

  lazy val networkChunkSize:Int = p2pSettings.get("networkChunkSize").flatMap(_.asNumber).flatMap(_.toInt)
    .getOrElse(DefaultNetworkChunkSize)

  lazy val localOnly: Boolean = p2pSettings.get("localOnly").flatMap(_.asBoolean).getOrElse(false)

  lazy val knownPeers: Seq[InetSocketAddress] = Try {
    p2pSettings.get("knownPeers").flatMap(_.asArray).map(_.flatMap(_.asString)).map(_.map { addr =>
      val addrParts = addr.split(":")
      val port = if (addrParts.size == 2) addrParts(1).toInt else DefaultPort
      new InetSocketAddress(addrParts(0), port)
    })
  }.toOption.flatten.getOrElse(Seq[InetSocketAddress]())

  lazy val bindAddress: String = p2pSettings.get("bindAddress").flatMap(_.asString).getOrElse(DefaultBindAddress)
  lazy val maxConnections: Int = p2pSettings.get("maxConnections")
    .flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultMaxConnections)
  lazy val connectionTimeout: Int = p2pSettings.get("connectionTimeout")
    .flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultConnectionTimeout)
  lazy val upnpEnabled: Boolean = p2pSettings.get("upnp").flatMap(_.asBoolean).getOrElse(true)
  lazy val upnpGatewayTimeout: Option[Int] = p2pSettings.get("upnpGatewayTimeout").flatMap(_.asNumber).flatMap(_.toInt)
  lazy val upnpDiscoverTimeout: Option[Int] = p2pSettings.get("upnpDiscoverTimeout").flatMap(_.asNumber).flatMap(_.toInt)
  lazy val port: Int = p2pSettings.get("port").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultPort)
  lazy val declaredAddress: Option[String] = p2pSettings.get("myAddress").flatMap(_.asString)

  lazy val handshakeTimeout: Int = p2pSettings.get("handshakeTimeout")
    .flatMap(_.asNumber)
    .flatMap(_.toInt)
    .getOrElse(DefaultHandshakeTimeout)

  lazy val rpcPort: Int = settingsJSON.get("rpcPort").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultRpcPort)

  lazy val blockGenerationDelay: FiniteDuration = settingsJSON.get("blockGenerationDelay").flatMap(_.asNumber).flatMap(_.toLong)
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(DefaultBlockGenerationDelay)

  lazy val miningThreads: Int = settingsJSON.get("miningThreads").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultMiningThreads)

  lazy val walletPassword: String = settingsJSON.get("walletPassword").flatMap(_.asString).getOrElse {
    scala.io.StdIn.readLine()
  }

  lazy val walletSeed: Array[Byte] = settingsJSON.get("walletSeed").flatMap(_.asString).flatMap(s => Base58.decode(s).toOption)
    .getOrElse {
      val generated = scorex.utils.Random.randomBytes(PrivKeyLength)
      log.warn("No wallet seed provided: generated new one:" + Base58.encode(generated))
      generated
    }

  lazy val apiKeyHash: Option[Array[Byte]] = settingsJSON.get("apiKeyHash").flatMap(_.asString).flatMap(s => Base58.decode(s).toOption)

  lazy val corsAllowed: Boolean = settingsJSON.get("cors").flatMap(_.asBoolean).getOrElse(false)

  //NETWORK
  private val DefaultMaxConnections = 20
  private val DefaultConnectionTimeout = 60
  private val DefaultBindAddress = "127.0.0.1"
  private val DefaultNetworkChunkSize = 100

  //API
  private val DefaultRpcPort = 9085
  private val DefaultBlockGenerationDelay: FiniteDuration = 1.second
  private val DefaultMiningThreads: Int = 1

  //APPLICATION DATA
  lazy val agentName: String = settingsJSON.get("agent").flatMap(_.asString)
    .getOrElse(Random.alphanumeric.take(16).mkString)

  lazy val appVersion: ApplicationVersion = settingsJSON.get("version").flatMap(_.asArray)
    .map(_.flatMap(_.asNumber.flatMap(_.toInt))).map(_.toArray)
    .map(ints => ApplicationVersion(ints(0), ints(1), ints(2)))
    .getOrElse(ApplicationVersion(0, 0, 1))

}

object Settings {
  val VersionNumbers = 3 //a version is about 3 numbers e.g. 1.0.1
}
