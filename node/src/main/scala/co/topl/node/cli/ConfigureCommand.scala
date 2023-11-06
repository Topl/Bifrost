package co.topl.node.cli

import cats.ApplicativeThrow
import cats.data.OptionT
import cats.effect.Async
import cats.effect.std.{Console, Env}
import cats.implicits._
import co.topl.brambl.models.LockAddress
import co.topl.config.ApplicationConfig
import co.topl.consensus.models.BlockId
import co.topl.node.AbstractNodeApp
import co.topl.typeclasses.implicits._
import fs2.io.file.{Files, Path}
import io.circe._
import io.circe.syntax._

import java.nio.charset.StandardCharsets

object ConfigureCommand {

  def apply[F[_]: Async: Console](appConfig: ApplicationConfig): StageResultT[F, Unit] =
    new ConfigureCommandImpl[F](appConfig).command

}

class ConfigureCommandImpl[F[_]: Async: Console](appConfig: ApplicationConfig) {

  private val intro: StageResultT[F, Unit] =
    writeMessage[F]("This utility helps configure your node.")

  private val promptDataDir =
    writeMessage[F](
      s"Where should blockchain data be saved? (Leave blank to use default=${appConfig.bifrost.data.directory})"
    ) >>
    readInput[F].map(_.some.filterNot(_.isEmpty))

  private val promptStakingSettings: StageResultT[F, (Option[String], Option[LockAddress])] =
    readYesNo("Enable staking operations?", No.some)(
      ifYes = for {
        _ <- writeMessage[F](
          s"Where should staking data be saved? (Leave blank to use default=${appConfig.bifrost.staking.directory})"
        )
        stakingDir <- readInput[F].map(_.some.filterNot(_.isEmpty))
        rewardAddress <- readOptionalParameter[F, LockAddress](
          "Reward Address",
          List("ptetP7jshHTwEg9Fz9Xa1AmmzhYHDHo1zZRde7mnw3fddcXPjV14RPcgVgy7")
        )
      } yield (stakingDir, rewardAddress),
      ifNo = (none[String], none[LockAddress]).pure[StageResultT[F, *]]
    )

  private val promptGenesis =
    readYesNo("Configure genesis settings?", Yes.some)(
      ifYes = for {
        blockId <- readOptionalParameter[F, BlockId](
          "Genesis Block ID",
          List("b_EyNPwteBBfESqrLKPUQ3xRaxaDPESTTENKgMrkTMYGYa")
        )
        sourcePath <- readOptionalParameter[F, String](
          "Genesis Data Source",
          List("https://raw.githubusercontent.com/Topl/Genesis_Testnets/main/testnet2")
        )
      } yield (blockId, sourcePath),
      ifNo = (none[BlockId], none[String]).pure[StageResultT[F, *]]
    )

  private val promptSettings =
    for {
      dataDir                             <- promptDataDir
      (stakingDir, stakingRewardAddress)  <- promptStakingSettings
      (genesisBlockId, genesisSourcePath) <- promptGenesis
    } yield ConfigureCommandInput(dataDir, stakingDir, stakingRewardAddress, genesisBlockId, genesisSourcePath)

  private def printConfig(configContents: String) =
    writeMessage[F]("Configuration contents:") >> writeMessage[F](configContents)

  private val promptDestination =
    StageResultT
      .liftF(OptionT(Env.make[F].get(AbstractNodeApp.ConfigFileEnvironmentVariable)).fold(Path("./user.yaml"))(Path(_)))
      .flatMap(default =>
        readDefaultedOptional[F, String]("Save location", List(default.toString, "/conf/app.yaml"), default.toString)
      )
      .map(Path(_))

  private def saveConfig(destination: Path, configContents: String): StageResultT[F, Unit] = {
    val w = writeFile(destination.parent.getOrElse(Path(".")))(configContents.getBytes(StandardCharsets.UTF_8))(
      "Config File",
      destination.fileName.toString
    )
    StageResultT
      .liftF(Files.forAsync[F].exists(destination))
      .ifM(
        readYesNo("Destination file exists.  Merge-override contents?", Yes.some)(
          ifYes = StageResultT
            .liftF(Files.forAsync[F].readUtf8(destination).compile.foldMonoid)
            .flatMap(mergeConfigs(_, configContents)),
          ifNo = configContents.pure[StageResultT[F, *]]
        ),
        configContents.pure[StageResultT[F, *]]
      )
      .map(_.getBytes(StandardCharsets.UTF_8))
      .flatMap(
        writeFile(destination.parent.getOrElse(Path(".")))(_)(
          "Config File",
          destination.fileName.toString
        )
      )

  }

  private def outro(configFile: Path): StageResultT[F, Unit] =
    writeMessage[F](s"Configuration complete.") >>
    writeMessage[F](s"The node can be launched by passing the following argument at launch: --config $configFile")

  private val outroNoConfig =
    writeMessage[F](s"No configuration required. The node can be launched without arguments.")

  private def mergeConfigs(existingConfigContents: String, newConfigContents: String): StageResultT[F, String] =
    StageResultT.liftF(
      ApplicativeThrow[F]
        .fromEither(
          (io.circe.yaml.parser.parse(existingConfigContents), io.circe.yaml.parser.parse(newConfigContents))
            .mapN(_.deepMerge(_))
        )
        .map(io.circe.yaml.printer.pretty)
    )

  val command: StageResultT[F, Unit] =
    for {
      _        <- intro
      settings <- promptSettings
      _ <- OptionT
        .fromOption[StageResultT[F, *]](settings.toYaml)
        .foldF(outroNoConfig)(configYaml =>
          for {
            _               <- printConfig(configYaml)
            saveDestination <- promptDestination
            _               <- saveConfig(saveDestination, configYaml)
            _               <- outro(saveDestination)
          } yield ()
        )
    } yield ()
}

private[cli] case class ConfigureCommandInput(
  dataDir:              Option[String],
  stakingDir:           Option[String],
  stakingRewardAddress: Option[LockAddress],
  genesisBlockId:       Option[BlockId],
  genesisSourcePath:    Option[String]
) {

  def toJson: Option[Json] =
    Json
      .obj(
        "bifrost" -> Json
          .obj(
            "data" -> Json
              .obj(
                "dir" -> dataDir.asJson
              )
              .dropNullValues,
            "staking" -> Json
              .obj(
                "dir"           -> stakingDir.asJson,
                "rewardAddress" -> stakingRewardAddress.map(co.topl.brambl.codecs.AddressCodecs.encodeAddress).asJson
              )
              .dropNullValues,
            "big-bang" -> Json
              .obj(
                "type"        -> genesisBlockId.void.orElse(genesisSourcePath.void).as("public").asJson,
                "genesis-id"  -> genesisBlockId.map(_.show).asJson,
                "source-path" -> genesisSourcePath.asJson
              )
              .dropNullValues
          )
          .dropEmptyValues
      )
      .dropEmptyValues
      .asObject
      .filter(_.nonEmpty)
      .map(_.asJson)

  def toYaml: Option[String] =
    toJson.map(io.circe.yaml.printer.pretty)
}
