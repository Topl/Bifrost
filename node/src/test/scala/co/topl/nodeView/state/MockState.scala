package co.topl.nodeView.state

import akka.actor.ActorSystem
import co.topl.attestation.Address
import co.topl.consensus.genesis.PrivateGenesis
import co.topl.keyManagement.{KeyRing, KeyfileCurve25519, PrivateKeyCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.utils.{CoreGenerators, FileUtils}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.util.{Failure, Success}

trait MockState
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators
    with FileUtils {

//  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
//  implicit val executionContext: ExecutionContext = actorSystem.dispatcher

//  protected val appContext = new AppContext(settings, StartupOpts.empty, None)

  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing.empty(settings.application.keyFileDir)

  keyRing.generateNewKeyPairs(num = 3) match {
    case Success(_)     => ()
    case Failure(error) => throw error
  }

  val genesisBlock: Block = PrivateGenesis(keyRing.addresses, settings).getGenesisBlock.get._1

  val genesisBlockId: ModifierId = genesisBlock.id

  def createState(genesisBlockWithVersion: Block = genesisBlock): State = {
    val file = createTempFile
    val tempSettings = settings.copy(
      application = settings.application.copy(
        dataDir = Some(file.getPath + "data")
      )
    )

    State.genesisState(tempSettings, Seq(genesisBlockWithVersion))
  }
}
