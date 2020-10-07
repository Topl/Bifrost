package example

import com.typesafe.config.Config
import settings.{AppSettings, StartupOpts}

trait GjallarhornGenerators {
  private val settingsFilename = "gjallarhorn/src/test/resources/test.conf"
  val settings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))
  val config: Config = AppSettings.readConfig(StartupOpts(Some(settingsFilename), None))

  private val requestSettingsFile = "gjallarhorn/src/test/resources/requestTest.conf"
  val requestSettings: AppSettings = AppSettings.read(StartupOpts(Some(requestSettingsFile), None))
  val requestConfig: Config = AppSettings.readConfig(StartupOpts(Some(requestSettingsFile), None))
}
