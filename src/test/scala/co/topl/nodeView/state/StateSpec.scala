package co.topl.nodeView.state

import co.topl.consensus.KeyRing
import co.topl.consensus.genesis.PrivateTestnet
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.state.StateSpec.block
import co.topl.settings.{AppSettings, StartupOpts}
import co.topl.utils.{CoreGenerators, FileUtils, ValidGenerators}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.util.{Failure, Success}

class StateSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators
  with FileUtils
  with BeforeAndAfterAll {

  def createState(settingsPath: String): State = {
    val file = createTempFile
    val initialSettings: AppSettings = AppSettings.read(StartupOpts(Some(settingsPath), None))
    val settings = initialSettings.copy(
      application = initialSettings.application.copy(
        dataDir = Some(file.getPath + "data")
      ))

    State.genesisState(settings, Seq(block))
  }
}

object StateSpec {

  val settingsFilename = "src/test/resources/test.conf"
  lazy val testSettings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))

  val keyRing: KeyRing = KeyRing(testSettings.application.keyFileDir.get)
  val block: Block = PrivateTestnet(( _: Int) => {
    keyRing.generateNewKeyPairs(num = 3) match {
      case Success(keys) => keys.map(_.publicImage)
      case Failure(ex)   => throw ex
    } }, testSettings).getGenesisBlock.get._1

  def genesisState(): State = State.genesisState(testSettings, Seq(block)).copy()

  val genesisBlockId: ModifierId = block.id

}