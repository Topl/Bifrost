package bifrost.state

import java.io.File
import java.time.Instant

import bifrost.{BifrostGenerators, BifrostNodeViewHolder, ValidGenerators}
import com.google.common.primitives.{Bytes, Ints, Longs}
import bifrost.blocks.BifrostBlock
import bifrost.forging.ForgingSettings
import bifrost.transaction._
import bifrost.transaction.box._
import bifrost.wallet.{BWallet, PolyTransferGenerator}
import io.circe
import org.scalacheck.Gen
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.{Failure, Random, Try}

class BifrostStateSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators
  with BeforeAndAfterAll{

  val settingsFilename = "settings.json"
  lazy val testSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  val gs = BifrostNodeViewHolder.initializeGenesis(testSettings)
  val history = gs._1; var genesisState = gs._2; var gw = gs._3
  // Generate new secret
  gw.generateNewSecret(); gw.generateNewSecret()
  val genesisBlockId = genesisState.version


  //noinspection ScalaStyle
  property("A block with valid PolyTransfer should result in more funds for receiver, less for transferrer") {
    // Create genesis block, add to state
    // Create new block with PolyTransfer
    // send new block to state
    // check updated state
    val beforePolyBoxes = gw.boxes().filter(_.box match {
      case a: PolyBox => genesisState.closedBox(a.id).isDefined
      case _ => false
    }).map(_.box.asInstanceOf[PolyBox])
    val beforeBoxKeys = beforePolyBoxes.flatMap(b => gw.secretByPublicImage(b.proposition).map(s => (b, s)))
    assert(beforeBoxKeys.map(_._1.value).sum == 100000000L)

    forAll(Gen.choose(0, 500)) { num: Int =>
      val poT = PolyTransferGenerator.generateStatic(gw).get
      val block = BifrostBlock(
        Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
        Instant.now().toEpochMilli,
        ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
        Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
        Seq(poT)
      )

      require(genesisState.validate(poT).isSuccess)

      val newState = genesisState.applyChanges(genesisState.changes(block).get, Ints.toByteArray(2)).get
      val newWallet = gw.scanPersistent(block)

      val arbitBoxes = newWallet.boxes().filter(_.box match {
        case a: ArbitBox => newState.closedBox(a.id).isDefined
        case _ => false
      }).map(_.box.asInstanceOf[ArbitBox])

      val boxKeys = arbitBoxes.flatMap(b => newWallet.secretByPublicImage(b.proposition).map(s => (b, s)))
      require(boxKeys.map(_._1.value).sum == 100000000L)

      val polyBoxes = newWallet.boxes().filter(_.box match {
        case a: PolyBox => newState.closedBox(a.id).isDefined
        case _ => false
    }).map(_.box.asInstanceOf[PolyBox])
      val polyBoxKeys = polyBoxes.flatMap(b => newWallet.secretByPublicImage(b.proposition).map(s => (b, s)))
      // The resulting poly balance = genesis amount - fee, because the transaction is sent to self
      // TODO: No fee is actually collected due to the reward box is an ArbitBox
      require(polyBoxKeys.map(_._1.value).sum == 100000000L - poT.fee)

      genesisState = newState.rollbackTo(genesisBlockId).get
      gw = newWallet.rollback(genesisBlockId).get
    }
  }

  property("A block with valid ProfileTransaction should result in a ProfileBox") {
    val timestamp = System.currentTimeMillis()
    val role = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub)).head
    val signature = PrivateKey25519Companion.sign(gw.secrets.head,
      ProfileTransaction.messageToSign(timestamp, gw.secrets.head.publicImage,
        Map("role" -> role.toString)))
    val tx = ProfileTransaction(gw.secrets.head.publicImage, signature, Map("role" -> role.toString), 0L, timestamp)

    val block = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(tx)
    )

    require(genesisState.validate(tx).isSuccess)

    val newState = genesisState.applyChanges(genesisState.changes(block).get, Ints.toByteArray(4)).get
    val box = newState.closedBox(FastCryptographicHash(gw.secrets.head.publicKeyBytes ++ "role".getBytes)).get.asInstanceOf[ProfileBox]

    genesisState = newState.rollbackTo(genesisBlockId).get

    box.key shouldBe "role"
    box.value shouldBe role.toString
  }

  property("Attempting to validate a PolyTransfer for amount you do not have should error") {

  }

  override def afterAll() {
    history.storage.storage.close()
    val path: Path = Path ("/tmp")
    Try(path.deleteRecursively())
  }
}