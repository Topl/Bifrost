package co.topl.nodeView.state

import akka.actor.ActorSystem
import co.topl.attestation.PrivateKeyCurve25519
import co.topl.consensus.KeyRing
import co.topl.consensus.genesis.PrivateTestnet
import co.topl.crypto.KeyfileCurve25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.settings.{AppContext, RuntimeOpts, StartupOpts}
import co.topl.utils.{CoreGenerators, FileUtils, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

trait MockState extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators
  with FileUtils {

  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  protected val appContext = new AppContext(settings, StartupOpts.empty, None)

  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing(settings.application.keyFileDir.get, KeyfileCurve25519)

  val genesisBlock: Block = PrivateTestnet((_: Int, _: Option[String]) => {
    keyRing.generateNewKeyPairs(num = 3) match {
      case Success(keys) => keys.map(_.publicImage)
      case Failure(ex)   => throw ex
    } }, settings).getGenesisBlock.get._1

  val genesisBlockId: ModifierId = genesisBlock.id

  def createState(genesisBlockWithVersion: Block = genesisBlock): State = {
    val file = createTempFile
    val tempSettings = settings.copy(
      application = settings.application.copy(
        dataDir = Some(file.getPath + "data")
      ))

    State.genesisState(tempSettings, Seq(genesisBlockWithVersion))
  }
}
