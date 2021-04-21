import com.typesafe.config.Config
import settings.{AppSettings, StartupOpts}

trait GjallarhornGenerators {
  private val settingsFilename = "gjallarhorn/src/test/resources/test.conf"
  val config: Config = AppSettings.readConfig(AppSettings.readFile(StartupOpts(Some(settingsFilename), None)))
  val settings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))

  private val requestSettingsFile = "gjallarhorn/src/test/resources/requestTest.conf"
  val requestConfig: Config = AppSettings.readConfig(AppSettings.readFile(StartupOpts(Some(requestSettingsFile), None)))
  val requestSettings: AppSettings = AppSettings.read(StartupOpts(Some(requestSettingsFile), None))

  private val keyManagementSettingsFile = "gjallarhorn/src/test/resources/keyManagementTest.conf"
  val keysConfig: Config = AppSettings.readConfig(AppSettings
    .readFile(StartupOpts(Some(keyManagementSettingsFile), None)))
  val keyManagementSettings: AppSettings = AppSettings.read(StartupOpts(Some(keyManagementSettingsFile), None))


}
