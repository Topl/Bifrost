package settings

import java.io.File

trait SettingsReaders {

  implicit val fileReader: ValueReader[File] = (cfg, path) => new File(cfg.getString(path))
}
