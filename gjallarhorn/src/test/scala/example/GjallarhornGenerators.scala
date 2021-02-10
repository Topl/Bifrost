package example

import com.typesafe.config.Config
import settings.{AppSettings, StartupOpts}

trait GjallarhornGenerators {
  private val settingsFilename = "gjallarhorn/src/test/resources/test.conf"
  val config: Config = AppSettings.readConfig(StartupOpts(Some(settingsFilename), None))
  val settings: AppSettings = AppSettings.fromConfig(config)

  private val requestSettingsFile = "gjallarhorn/src/test/resources/requestTest.conf"
  val requestConfig: Config = AppSettings.readConfig(StartupOpts(Some(requestSettingsFile), None))
  val requestSettings: AppSettings = AppSettings.fromConfig(requestConfig)

  private val keyManagementSettingsFile = "gjallarhorn/src/test/resources/keyManagementTest.conf"
  val keysConfig: Config = AppSettings.readConfig(StartupOpts(Some(keyManagementSettingsFile), None))
  val keyManagementSettings: AppSettings = AppSettings.fromConfig(keysConfig)


}
