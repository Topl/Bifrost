package co.topl.it

import com.typesafe.config.{Config, ConfigFactory}

import scala.jdk.CollectionConverters.asScalaBufferConverter

trait IntegrationConstants {

  val nodesJointConfig: Config = ConfigFactory.parseResources("nodes.conf").resolve()
  val nodeSeedConfigs: List[Config] = nodesJointConfig.getConfigList("nodes").asScala.toList
}
