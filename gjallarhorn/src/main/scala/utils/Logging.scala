package utils

import com.typesafe.scalalogging.{Logger, StrictLogging}

trait Logging extends StrictLogging {
  protected def log: Logger = logger
}
