package bifrost.utils

import com.typesafe.scalalogging.StrictLogging

trait ScorexLogging extends StrictLogging {
  protected def log = logger
}
