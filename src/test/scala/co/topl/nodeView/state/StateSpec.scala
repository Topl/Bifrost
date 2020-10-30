package co.topl.nodeView.state

import co.topl.consensus.KeyRing
import co.topl.consensus.genesis.PrivateTestnet
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
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

  val keyRing: KeyRing = KeyRing(settings.application.keyFileDir.get)

  val genesisBlock: Block = PrivateTestnet(( _: Int) => {
    keyRing.generateNewKeyPairs(num = 3) match {
      case Success(keys) => keys.map(_.publicImage)
      case Failure(ex)   => throw ex
    } }, settings).getGenesisBlock.get._1

  val genesisBlockId: ModifierId = genesisBlock.id

  def createState(): State = {
    val file = createTempFile
    val tempSettings = settings.copy(
      application = settings.application.copy(
        dataDir = Some(file.getPath + "data")
      ))

    State.genesisState(tempSettings, Seq(genesisBlock))
  }
}
