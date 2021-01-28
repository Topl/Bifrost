package keymanager

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Status}
import akka.util.Timeout
import attestation.{Address, PrivateKeyCurve25519}
import crypto.KeyfileCurve25519
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58
import settings.{ApplicationSettings, NetworkType}
import utils.Logging
import wallet.WalletManager
import wallet.WalletManager.NewKey

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._


class KeyManager(settings: ApplicationSettings) extends Actor with Logging {

  import KeyManager._

  private var keyRing: Keys[PrivateKeyCurve25519, KeyfileCurve25519] =
    Keys(settings.keyFileDir, KeyfileCurve25519)(PrivateKeyCurve25519.secretGenerator, networkPrefix = networkPrefix)

  implicit val timeout: Timeout = 10.seconds

  override def receive: Receive = {
    case GenerateKeyFile(password, seedOpt) => shareNewKey(keyRing.generateKeyFile(password, seedOpt), sender())

    case ImportKeyfile(password: String, mnemonic: String, lang: String) =>
      shareNewKey(keyRing.importPhrase(password, mnemonic, lang), sender())

    case UnlockKeyFile(addressString, password) => sender ! keyRing.unlockKeyFile(addressString, password)

    case LockKeyFile(addressString) => sender ! keyRing.lockKeyFile(addressString)

    case GetOpenKeyfiles => sender ! keyRing.addresses

    case GetAllKeyfiles => sender ! keyRing.listKeyFilesAndStatus

    case SignTx(tx: Json, keys: IndexedSeq[Address], msg: String) =>
      val newTx = signTx(tx, keys, msg)
        sender ! Map("tx" -> newTx).asJson

    case GenerateSignatures(keys: IndexedSeq[Address], msg: String) =>
      sender ! Map("signatures" -> createSignatures(keys, msg)).asJson

    case ChangeNetwork(networkName: String) => sender ! switchNetwork(networkName)

    case GetKeyfileDir => sender ! Map("keyfileDirectory" -> keyRing.getNetworkDir.getAbsolutePath).asJson

    case ChangeKeyfileDir(dir: String) =>
      updateKeyfileDir(settings.keyFileDir, dir)
      settings.keyFileDir = dir
      keyRing = Keys(settings.keyFileDir, KeyfileCurve25519)(PrivateKeyCurve25519.secretGenerator,
        networkPrefix = networkPrefix)
      sender ! Map("newDirectory" -> settings.keyFileDir).asJson
  }

  private def shareNewKey(newAddress: Try[Address], sender: ActorRef) {
    newAddress match {
      case Success(addr) =>
        context.actorSelection("../" + WalletManager.actorName).resolveOne().onComplete {
          case Success(walletActor) => walletActor ! NewKey(addr)
          case Failure(exception) => log.info("offline mode")
        }
      case Failure(ex) => log.error("unable to generate key file!")
    }
    sender ! newAddress
  }

  //TODO: should this path be hardcoded?
  private def updateKeyfileDir(oldDir: String, newDir: String): Unit = {
    val path = "gjallarhorn/src/main/resources/application.conf"
    val configFile: File = new File(path)
    if (!configFile.exists()) {
      throw new Error (s"The config file: $path does not exist!")
    }

    var lines: Array[String] = Array.empty
    val reader = new BufferedReader(new FileReader(configFile))
    var line: String = ""
    while ({line = reader.readLine; line != null}) {
      if (line.contains("keyFileDir")) {
        val newLine = line.replace(oldDir, newDir)
        lines = lines :+ newLine
      }else{
        lines = lines :+ line
      }
    }
    reader.close()

    val writer = new BufferedWriter(new FileWriter(configFile))
    lines.foreach(line => {
      writer.write(line)
      writer.newLine()
    })
    writer.close()
  }

  private def createSignatures (keys: IndexedSeq[Address], msg: String): Map[PrivateKeyCurve25519#PK, Json] = {
    keys.map(address => {
      Base58.decode(msg) match {
        case Success(msgToSign) =>
          keyRing.signWithAddress(address, msgToSign) match {
            case Success(signedTx) =>
              val sig = signedTx.asJson
              keyRing.lookupPublicKey(address) match {
                case Success(pubKey) => pubKey -> sig
                case Failure(exception) => throw exception
              }
            case Failure(exception) => throw exception
          }
        case Failure(exception) => throw exception
      }
    }).toMap
  }

  private def signTx(tx: Json, keys: IndexedSeq[Address], msg: String): Json = {
    (for {
      currentSignatures <- (tx \\ "signatures").head.as[Map[PrivateKeyCurve25519#PK, Json]]
    } yield {
      val newSignatures: Map[PrivateKeyCurve25519#PK, Json] = createSignatures(keys, msg)
      tx.deepMerge(Map(
        "signatures" -> (currentSignatures ++ newSignatures).asJson
      ).asJson)
    }) match {
      case Right(value) => value
      case Left(ex) => throw new Exception(s"error parsing json: $ex")
    }
  }

  private def switchNetwork(networkName: String): Try[Json] = Try{
    NetworkType.fromString(networkName) match {
      case Some(network) =>
        if (network.netPrefix != networkPrefix) {
          //lock all keyfiles on current network
          keyRing.addresses.foreach(addr =>
            keyRing.lockKeyFile(addr.toString))
          //change network and initialize keyRing with new network
          networkPrefix = network.netPrefix
          keyRing = Keys(settings.keyFileDir, KeyfileCurve25519)(PrivateKeyCurve25519.secretGenerator,
            networkPrefix = networkPrefix)
          log.info(s"${Console.MAGENTA}Network changed to: ${network.verboseName} ${Console.RESET}")
        }
        Map("newNetworkPrefix" -> networkPrefix).asJson
      case None => throw new Exception(s"The network name: $networkName was not a valid network type!")
    }}
}

object KeyManager {
  case class GenerateKeyFile(password: String, seedOpt: Option[String])
  case class ImportKeyfile(password: String, mnemonic: String, lang: String)
  case class UnlockKeyFile(addressString: String, password: String)
  case class LockKeyFile(addressString: String)
  case object GetOpenKeyfiles
  case object GetAllKeyfiles
  case class SignTx(transaction: Json, signingKeys: IndexedSeq[Address], messageToSign: String)
  case class GenerateSignatures(signingKeys: IndexedSeq[Address], messageToSign: String)
  case class ChangeNetwork(networkName: String)
  case object GetKeyfileDir
  case class ChangeKeyfileDir(dir: String)
}

object KeyManagerRef {

  def props(settings: ApplicationSettings)
           (implicit ec: ExecutionContext): Props = {
    Props(new KeyManager(settings))
  }

  def apply(name: String, settings: ApplicationSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(Props(new KeyManager(settings)), name = name)
  }
}