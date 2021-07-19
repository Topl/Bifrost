package co.topl.nodeView.state

import akka.actor.ActorSystem
import co.topl.attestation.Address
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.consensus.genesis.PrivateGenesis
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.utils.{CommonGenerators, FileUtils, NodeGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

trait MockState
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CommonGenerators
    with NodeGenerators
    with FileUtils {

  def createState(genesisBlockWithVersion: Block = genesisBlock, inTempFile: Boolean = true): State = {
    val finalSettings = if (inTempFile) {
      val file = createTempFile
      settings.copy(
        application = settings.application.copy(
          dataDir = Some(file.getPath + "data")
        )
      )
    } else {
      settings
    }
    State.genesisState(finalSettings, Seq(genesisBlockWithVersion))
  }
}
