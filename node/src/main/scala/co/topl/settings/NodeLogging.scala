package co.topl.settings

import ch.qos.logback.classic.{Level, LoggerContext}
import com.typesafe.scalalogging.{LazyLogging, Logger}
import org.slf4j.LoggerFactory

trait NodeLogging extends LazyLogging {

  def log: Logger = logger

  def setLogLevel(): Unit =
    LoggerFactory.getILoggerFactory
      .asInstanceOf[LoggerContext]
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .setLevel(Level.DEBUG)
}
