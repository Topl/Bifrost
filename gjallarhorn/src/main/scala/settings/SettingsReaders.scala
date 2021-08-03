package settings

import net.ceedubs.ficus.readers.ValueReader

import java.io.File

trait SettingsReaders {

  implicit val fileReader: ValueReader[File] = (cfg, path) => new File(cfg.getString(path))
}
