package co.topl.node

import ch.qos.logback.classic.joran.JoranConfigurator

object LoggingUtils {

  /**
   * Override the runtime's logback configuration using an optional logback file defined in the provided args
   */
  def initialize(args: Args): Unit =
    args.startup.logbackFile
      .foreach { startupFileName =>
        val context = org.slf4j.LoggerFactory.getILoggerFactory().asInstanceOf[ch.qos.logback.classic.LoggerContext]
        val configurator = new JoranConfigurator
        context.reset()
        configurator.setContext(context)
        configurator.doConfigure(startupFileName)
      }
}
