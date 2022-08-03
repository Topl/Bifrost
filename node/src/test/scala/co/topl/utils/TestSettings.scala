package co.topl.utils

import co.topl.consensus.ProtocolVersioner
import co.topl.settings.{AppContext, AppSettings, StartupOpts}
import com.typesafe.config.Config

import java.nio.file.Files

trait TestSettings extends KeyRingTestHelper {
  implicit def settings: AppSettings = TestSettings.defaultSettings
  implicit def appContext: AppContext = TestSettings.defaultAppContext
  implicit def protocolVersioner: ProtocolVersioner = TestSettings.protocolVersioner
}

object TestSettings {
  private val settingsFilename = "node/src/test/resources/application-test.conf"

  val (defaultSettings: AppSettings, defaultConfig: Config) = {
    val (s, c) = AppSettings.read(StartupOpts(Some(settingsFilename)))
    s.copy(
      application = s.application.copy(
        dataDir = Some(Files.createTempDirectory("bifrost-test-data").toString),
        consensusStoreVersionsToKeep = 10
      )
    ) -> c
  }

  val defaultAppContext: AppContext =
    new AppContext(defaultSettings, StartupOpts(), None)

  val protocolVersioner: ProtocolVersioner =
    ProtocolVersioner(defaultSettings.application.version, defaultSettings.forging.protocolVersions)
}
