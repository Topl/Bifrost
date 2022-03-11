package utils

trait Logging extends StrictLogging {
  protected def log: Logger = logger
}
