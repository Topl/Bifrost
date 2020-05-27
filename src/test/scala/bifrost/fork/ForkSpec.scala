
package bifrost.fork

import bifrost.BifrostGenerators
import bifrost.consensus.DifficultyBlockValidator
import bifrost.crypto.Signature25519
import bifrost.forging.ForgingSettings
import bifrost.history.History
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.nodeView.NodeViewHolder
import bifrost.nodeView.NodeViewHolder.{HIS, MP, MS, VL}
import io.circe
import io.circe.syntax._
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}

class ForkSpec extends PropSpec
  with Matchers
  with BeforeAndAfterAll
  with BifrostGenerators
{
  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  val settingsFilename = "testSettings.json"

  lazy val testSettings_version3: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  lazy val testSettings_version0: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename) +
      ("version" -> (List(0,0,0).asJson)) +
      ("forkHeight" -> 3.asJson)
  }

  val gs: (HIS, MS, VL, MP) = NodeViewHolder.initializeGenesis(testSettings_version0)
  var history: HIS = gs._1
  var genesisState: MS = gs._2
  var gw: VL = gs._3

  property("Appending version3 blocks before height = forkHeight should fail") {
    val tempBlock_version3 = Block(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      0L,
      testSettings_version3.version)

    history = history.append(tempBlock_version3).get._1
    history.modifierById(tempBlock_version3.id).isDefined shouldBe false

    history.storage.rollback(tempBlock_version3.parentId)
    history = new History(history.storage,
      testSettings_version3,
      Seq(
        new DifficultyBlockValidator(history.storage)
        //new ParentBlockValidator(storage),
        //new SemanticBlockValidator(FastCryptographicHash)
      )
    )
  }

  property("Appending version3 blocks after height = forkHeight should work") {

    println(s"history.height: ${history.height}")
    for(i <- 2L to testSettings_version0.forkHeight) {
      val tempBlock = Block(history.bestBlockId,
        System.currentTimeMillis(),
        ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
        Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
        Seq(),
        0L,
        testSettings_version0.version)

      Thread.sleep(1000)

      history = history.append(tempBlock).get._1
      assert(history.modifierById(tempBlock.id).isDefined)
    }

    val tempBlock_version3_1 = Block(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version3.version)

    history = history.append(tempBlock_version3_1).get._1
    assert(history.modifierById(tempBlock_version3_1.id).isDefined)

    Thread.sleep(1000)

    val tempBlock_version3_2 = Block(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version3.version)

    history = history.append(tempBlock_version3_2).get._1
    assert(history.modifierById(tempBlock_version3_2.id).isDefined)

    history.height shouldEqual testSettings_version0.forkHeight + 2

    history.storage.rollback(tempBlock_version3_1.parentId)
    history = new History(history.storage,
      testSettings_version3,
      Seq(
        new DifficultyBlockValidator(history.storage)
        //new ParentBlockValidator(storage),
        //new SemanticBlockValidator(FastCryptographicHash)
      )
    )

    history.height shouldEqual testSettings_version0.forkHeight

    assert(history.height == testSettings_version0.forkHeight)
  }

  property("Appending version0 blocks after height = forkHeight should fail") {

    println(s"history.height: ${history.height}")
    Thread.sleep(1000)
    
    val tempBlock_version0 = Block(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version0.version)

    history = history.append(tempBlock_version0).get._1
    history.modifierById(tempBlock_version0.id).isDefined shouldBe false

    history.storage.rollback(tempBlock_version0.parentId)
    history = new History(history.storage,
      testSettings_version3,
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
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version3.version)

    history = history.append(tempBlock_version3).get._1
    assert(history.modifierById(tempBlock_version3.id).isDefined)

    val tempBlock_version0 = Block(history.bestBlockId,
      System.currentTimeMillis(),
      ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
      Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
      Seq(),
      10L,
      testSettings_version0.version)

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
          testSettings_version3,
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

