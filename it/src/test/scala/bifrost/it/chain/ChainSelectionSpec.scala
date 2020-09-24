package bifrost.it.chain

import bifrost.BifrostGenerators
import bifrost.it.IntegrationSuite
import bifrost.it.util.Node
import com.typesafe.config.Config
import org.scalatest.freespec.AnyFreeSpec


class ChainSelectionSpec extends AnyFreeSpec
  with IntegrationSuite
  with BifrostGenerators {

  val numNodes: Int = 2

  val offlineForgingNodes: List[Config] = nodeSeedConfigs.slice(1, numNodes)
  val onlineForgingNodes: List[Config] = nodeSeedConfigs.slice(1, numNodes)

  "Multiple chains reconcile to single canonical chain" in {

    val offlineNodes: Seq[Node] = docker.startNodes(offlineForgingNodes)
  }
}
