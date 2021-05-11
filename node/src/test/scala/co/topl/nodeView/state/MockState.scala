package co.topl.nodeView.state

import co.topl.modifier.block.Block
import co.topl.utils.{CoreGenerators, FileUtils, NodeGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

trait MockState
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators
    with NodeGenerators
    with FileUtils {

//  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
//  implicit val executionContext: ExecutionContext = actorSystem.dispatcher

//  protected val appContext = new AppContext(settings, StartupOpts.empty, None)

  def createState(genesisBlockWithVersion: Block = genesisBlock): State =
    State.genesisState(settings, Seq(genesisBlockWithVersion))
}
