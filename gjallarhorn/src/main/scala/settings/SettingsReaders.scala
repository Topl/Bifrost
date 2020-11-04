package settings

import java.io.File
import net.ceedubs.ficus.readers.ValueReader
import com.typesafe.config.Config

trait SettingsReaders {

  implicit val fileReader: ValueReader[File] = (cfg, path) => new File(cfg.getString(path))
}
