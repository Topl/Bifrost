package bifrost.utils

import com.typesafe.scalalogging.StrictLogging

trait Logging extends StrictLogging {
  protected def log = logger
}
