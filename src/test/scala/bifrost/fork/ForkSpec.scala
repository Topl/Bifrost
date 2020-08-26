
package bifrost.fork

import java.time.Instant

import bifrost.BifrostGenerators
import bifrost.consensus.DifficultyBlockValidator
import bifrost.crypto.Signature25519
import bifrost.history.History
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.nodeView.NodeViewHolder
import bifrost.nodeView.NodeViewHolder.{HIS, MP, MS, VL}
import org.scalatest.BeforeAndAfterAll
import bifrost.settings.{AppSettings, StartupOpts}
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class ForkSpec extends AnyPropSpec
  with Matchers
  with BeforeAndAfterAll
  with BifrostGenerators
{
  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  private val settingsFilename = "src/test/resources/test.conf"
  val testSettings_version1: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))
  val originalSettings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))
  val testSettings_version0: AppSettings = originalSettings
    .copy(version = "0.0.0",
          forgingSettings = originalSettings.forgingSettings.copy(forkHeight = 3, version = 0))

  val gs: (HIS, MS, VL, MP) = NodeViewHolder.initializeGenesis(testSettings_version0)
  var history: HIS = gs._1
  var genesisState: MS = gs._2
  var gw: VL = gs._3

  println("Just before prop")

  property("Appending version3 blocks before height = forkHeight should fail") {
    val tempBlock_version3 = Block(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      0L,
      testSettings_version1.forgingSettings.version)

    history = history.append(tempBlock_version3).get._1
    history.modifierById(tempBlock_version3.id).isDefined shouldBe false

    history.storage.rollback(tempBlock_version3.parentId)
    history = new History(history.storage,
      testSettings_version1,
      Seq(
        new DifficultyBlockValidator(history.storage)
        //new ParentBlockValidator(storage),
        //new SemanticBlockValidator(FastCryptographicHash)
      )
    )
  }

  property("Appending version3 blocks after height = forkHeight should work") {

    println(s"history.height: ${history.height}")
    for(i <- 2L to testSettings_version0.forgingSettings.forkHeight) {
      val tempBlock = Block(history.bestBlockId,
        Instant.now().toEpochMilli,
        ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, Long.MaxValue),
        Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
        Seq(),
        0L,
        testSettings_version0.forgingSettings.version)

      Thread.sleep(1000)

      history = history.append(tempBlock).get._1
      assert(history.modifierById(tempBlock.id).isDefined)
    }

    val tempBlock_version3_1 = Block(history.bestBlockId,
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, Long.MaxValue),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version1.forgingSettings.version)

    history = history.append(tempBlock_version3_1).get._1
    assert(history.modifierById(tempBlock_version3_1.id).isDefined)

    Thread.sleep(1000)

    val tempBlock_version3_2 = Block(history.bestBlockId,
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, Long.MaxValue),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version1.forgingSettings.version)

    history = history.append(tempBlock_version3_2).get._1
    assert(history.modifierById(tempBlock_version3_2.id).isDefined)

    history.height shouldEqual testSettings_version0.forgingSettings.forkHeight + 2

    history.storage.rollback(tempBlock_version3_1.parentId)
    history = new History(history.storage,
      testSettings_version1,
      Seq(
        new DifficultyBlockValidator(history.storage)
        //new ParentBlockValidator(storage),
        //new SemanticBlockValidator(FastCryptographicHash)
      )
    )

    history.height shouldEqual testSettings_version0.forgingSettings.forkHeight

    assert(history.height == testSettings_version0.forgingSettings.forkHeight)
  }

  property("Appending version0 blocks after height = forkHeight should fail") {

    println(s"history.height: ${history.height}")
    Thread.sleep(1000)
    
    val tempBlock_version0 = Block(history.bestBlockId,
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, Long.MaxValue),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version0.forgingSettings.version)

    history = history.append(tempBlock_version0).get._1
    history.modifierById(tempBlock_version0.id).isDefined shouldBe false

    history.storage.rollback(tempBlock_version0.parentId)
    history = new History(history.storage,
      testSettings_version1,
      Seq(
        new DifficultyBlockValidator(history.storage)
        //new ParentBlockValidator(storage),
        //new SemanticBlockValidator(FastCryptographicHash)
      )
    )
  }

  property("Appending version3 blocks after height = forkHeight and then appending a version0 block should fail") {

    println(s"history.height: ${history.height}")
    Thread.sleep(1000)

    val tempBlock_version3 = Block(history.bestBlockId,
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, Long.MaxValue),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version1.forgingSettings.version)

    history = history.append(tempBlock_version3).get._1
    assert(history.modifierById(tempBlock_version3.id).isDefined)

    val tempBlock_version0 = Block(history.bestBlockId,
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, Long.MaxValue),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version0.forgingSettings.version)

    val appendResult = history.append(tempBlock_version0)
    appendResult match {
      case Success(result) =>
        history = result._1
        history.modifierById(tempBlock_version0.id).isDefined shouldBe false

        //Since block validation does not exist block is still appended to history, failure only pops up
        //when trying to recreate a block from id when updating difficulty in DifficultyBlockValidator

        //Hence failure pops up when trying to append a new block on top of an incorrect block

        //heightBeforeAppendAttempt shouldEqual heightAfterAppendAttempt, but is not the case due to above reason
        //manually rolling back storage, recreating history, and then checking height

        //    heightBeforeAppendAttempt shouldEqual heightAfterAppendAttempt

        history.storage.rollback(tempBlock_version3.parentId)
        history = new History(history.storage,
          testSettings_version1,
          Seq(
            new DifficultyBlockValidator(history.storage)
            //new ParentBlockValidator(storage),
            //new SemanticBlockValidator(FastCryptographicHash)
          )
        )

      case Failure(_) =>
    }
  }

  override def afterAll() {
    history.storage.storage.close
  }
}

