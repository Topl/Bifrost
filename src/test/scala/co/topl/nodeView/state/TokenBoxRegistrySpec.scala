package co.topl.nodeView.state

import java.time.Instant

import co.topl.crypto.Signature25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.ArbitTransfer
import co.topl.nodeView.NodeViewHolder
import co.topl.nodeView.NodeViewHolder.{HIS, MP, MS, VL}
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.settings.{AppSettings, StartupOpts}
import co.topl.{BifrostGenerators, ValidGenerators}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.util.encode.Base58

import scala.reflect.io.Path
import scala.util.Try

class TokenBoxRegistrySpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BeforeAndAfterAll
  with BifrostGenerators
  with ValidGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  private val settingsFilename = "src/test/resources/test.conf"
  lazy val testSettings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))

  val gs: (HIS, MS, VL, MP) = NodeViewHolder.initializeGenesis(testSettings)
  val history: HIS = gs._1
  var genesisState: MS = gs._2
  var gw: VL = gs._3

  // Unlock Secrets
  gw.unlockKeyFile("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ", "genesis")
  gw.unlockKeyFile("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb", "genesis")
  gw.unlockKeyFile("F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU", "genesis")


  property("Transfer should update tokenBoxRegistry correctly") {

    val oldArbitBoxes = gw
      .boxesByKey("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
      .filter(_.box match {
        case a: ArbitBox => genesisState.getBox(a.id).isDefined
        case _ => false
      })
      .map(_.box.asInstanceOf[ArbitBox])
    assert(oldArbitBoxes.length == 1)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get).get.count(_.isInstanceOf[ArbitBox]) == 1)

    val tx1 = ArbitTransfer.create(
      genesisState.tbrOpt.get,
      genesisState,
      gw,
      IndexedSeq((PublicKey25519Proposition(PublicKey @@ Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get), 5L)),
      IndexedSeq(PublicKey25519Proposition(PublicKey @@ Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)),
      0,
      ""
    ).get

    val block1 = Block(
      ModifierId(Array.fill(Block.signatureLength)(-1: Byte)),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(PublicKey @@ Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Signature @@ Array.fill(Block.signatureLength)(0: Byte)),
      Seq(tx1), settings.forgingSettings.version)

    require(genesisState.validate(tx1).isSuccess)

    val newState1 = genesisState
      .applyModifier(block1)
      .get

    val newWallet1 = gw.scanPersistent(block1)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get).get.count(_.isInstanceOf[ArbitBox]) == 1)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get).get.count(_.isInstanceOf[ArbitBox]) == 1)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get).get
      .filter(_.isInstanceOf[ArbitBox]).head.value == 99999995)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get).get
      .filter(_.isInstanceOf[ArbitBox]).head.value == 5)

    val tx2 = ArbitTransfer.create(
      newState1.tbrOpt.get,
      newState1,
      newWallet1,
      IndexedSeq((PublicKey25519Proposition(PublicKey @@ Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get), 4L)),
      IndexedSeq(PublicKey25519Proposition(PublicKey @@ Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get)),
      0,
      "",
    ).get

    val block2 = Block(
      ModifierId(Array.fill(Block.signatureLength)(-1: Byte)),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(PublicKey @@ Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Signature @@ Array.fill(Block.signatureLength)(0: Byte)),
      Seq(tx2), settings.forgingSettings.version)

    require(newState1.validate(tx2).isSuccess)

    val newState2 = newState1
      .applyModifier(block2)
      .get

    val newWallet2 = newWallet1.scanPersistent(block2)


    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get).get.count(_.isInstanceOf[ArbitBox]) == 2)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get).get.count(_.isInstanceOf[ArbitBox]) == 1)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get).get
      .filter(_.isInstanceOf[ArbitBox]).forall(i => i.value == 99999995 || i.value == 4))

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get).get
      .filter(_.isInstanceOf[ArbitBox]).head.value == 1)

    newState2.rollbackTo(genesisState.version)
    newWallet2.rollback(genesisState.version)


  }

  property("Rollback should have worked and recreated above changes exactly") {

    val tx1 = ArbitTransfer.create(
      genesisState.tbrOpt.get,
      genesisState,
      gw,
      IndexedSeq((PublicKey25519Proposition(PublicKey @@ Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get), 5L)),
      IndexedSeq(PublicKey25519Proposition(PublicKey @@ Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)),
      0,
      "",
    ).get

    val block1 = Block(
      ModifierId(Array.fill(Block.signatureLength)(-1: Byte)),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(PublicKey @@ Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Signature @@ Array.fill(Block.signatureLength)(0: Byte)),
      Seq(tx1), settings.forgingSettings.version)

    require(genesisState.validate(tx1).isSuccess)

    val newState1 = genesisState
      .applyModifier(block1)
      .get

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get).get.count(_.isInstanceOf[ArbitBox]) == 1)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get).get.count(_.isInstanceOf[ArbitBox]) == 1)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get).get
      .filter(_.isInstanceOf[ArbitBox]).head.value == 99999995)

    assert(genesisState.getTokenBoxes(PublicKey25519Proposition("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get).get
      .filter(_.isInstanceOf[ArbitBox]).head.value == 5)

  }

  override def afterAll() {
    history.closeStorage()
    genesisState.closeStorage()
  }
}
