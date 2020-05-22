package bifrost.forging

import java.io.File

import bifrost.settings.Settings
import io.circe.syntax._

import scala.concurrent.duration._

trait ForgingSettings extends Settings {

  val InitialDifficulty = 15000000L
  val MinimumDifficulty = 100L
  lazy val GenesisParentId: Array[Byte] = Array.fill(32)(1: Byte)

  lazy val offlineGeneration = settingsJSON
    .get("offlineGeneration")
    .flatMap(_.asBoolean)
    .getOrElse(false)

  lazy val posAttachmentSize = settingsJSON
    .get("posAttachmentSize")
    .flatMap(_.asNumber)
    .flatMap(_.toInt)
    .getOrElse(DefaultPosAttachmentSize)

  lazy val targetBlockTime: FiniteDuration = settingsJSON
    .get("targetBlockTime")
    .flatMap(_.asNumber)
    .flatMap(_.toLong)
    .map(x => FiniteDuration(x, MILLISECONDS))
    .getOrElse(30.second)

  val DefaultPosAttachmentSize = 1024

  lazy val version = settingsJSON
    .get("version")
    .flatMap(_.asArray)
    .map(_.flatMap(_.asNumber.flatMap(_.toInt)))
    .map(_.toArray)
    .map(ints => ints(0).toByte)
    .getOrElse(0.toByte)


  lazy val forkHeight = settingsJSON
    .get("forkHeight")
    .flatMap(_.asNumber)
    .flatMap(_.toLong)
    .getOrElse(0L)

  private def folderOpt(settingName: String) = {
    val res = settingsJSON.get(settingName).flatMap(_.asString)
    res.foreach(folder => new File(folder).mkdirs())
    require(res.isEmpty || new File(res.get).exists())
    res
  }

  override def toString: String = (Map("BlockGenerationDelay" -> blockGenerationDelay.length.asJson) ++
    settingsJSON.map(s => s._1 -> s._2)).asJson.spaces2
}
