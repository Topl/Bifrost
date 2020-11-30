package co.topl.it.chain

import co.topl.BifrostGenerators
import co.topl.it.IntegrationSuite
import co.topl.it.util.Node
import co.topl.BifrostGenerators
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
