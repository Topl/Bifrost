package example

import settings.{AppSettings, StartupOpts}

trait GjallarhornGenerators {
  private val settingsFilename = "src/test/resources/test.conf"
  val settings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))
}
