package bifrost.state

import java.time.Instant

import bifrost.BifrostNodeViewHolder.{HIS, MP, MS, VL}
import bifrost.blocks.BifrostBlock
import bifrost.forging.ForgingSettings
import bifrost.state.BifrostStateSpec.gw
import bifrost.transaction.bifrostTransaction.{ArbitTransfer, AssetTransfer}
import bifrost.transaction.box.ArbitBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.{BifrostGenerators, BifrostNodeViewHolder, ValidGenerators}
import com.google.common.primitives.Ints
import io.circe
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.Try

class BFRSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BeforeAndAfterAll
  with BifrostGenerators
  with ValidGenerators {

  val path: Path = Path("/tmp/scorex/test-data")
  Try(path.deleteRecursively())

  val settingsFilename = "testSettings.json"
  lazy val testSettings: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  val gs: (HIS, MS, VL, MP) = BifrostNodeViewHolder.initializeGenesis(testSettings)
  val history: HIS = gs._1
  var genesisState: MS = gs._2
  var gw: VL = gs._3

  // Unlock Secrets
  gw.unlockKeyFile("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ", "genesis")
  gw.unlockKeyFile("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb", "genesis")
  gw.unlockKeyFile("F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU", "genesis")


  property("Transfer should update bfr correctly") {

    val oldArbitBoxes = gw
      .boxesByKey("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
      .filter(_.box match {
        case a: ArbitBox => genesisState.closedBox(a.id).isDefined
        case _ => false
      })
      .map(_.box.asInstanceOf[ArbitBox])
    assert(oldArbitBoxes.length == 1)

    assert(genesisState.bfr.boxesByKey(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get).filter(_.isInstanceOf[ArbitBox]).length == 1)

    val tx1 = ArbitTransfer.create(genesisState.bfr,
      gw,
      IndexedSeq((PublicKey25519Proposition(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get), 5L)),
      IndexedSeq(PublicKey25519Proposition(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)),
      0,
      ""
    ).get

    val block1 = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(tx1), 10L, settings.version)

    require(genesisState.validate(tx1).isSuccess)

    val newState1 = genesisState
      .applyChanges(genesisState.changes(block1).get, block1.id)
      .get

    val newWallet1 = gw.scanPersistent(block1)

    assert(newState1.bfr.boxesByKey(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)
      .filter(_.isInstanceOf[ArbitBox]).length == 1)

    assert(newState1.bfr.boxesByKey(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get)
      .filter(_.isInstanceOf[ArbitBox]).length == 1)

    assert(newState1.bfr.boxesByKey(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)
      .filter(_.isInstanceOf[ArbitBox]).head.value == 99999995)

    assert(newState1.bfr.boxesByKey(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get)
      .filter(_.isInstanceOf[ArbitBox]).head.value == 5)

    val tx2 = ArbitTransfer.create(newState1.bfr,
      newWallet1,
      IndexedSeq((PublicKey25519Proposition(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get), 4L)),
      IndexedSeq(PublicKey25519Proposition(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get)),
      0,
      "",
    ).get

    val block2 = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(tx2), 10L, settings.version)

    require(newState1.validate(tx2).isSuccess)

    val newState2 = newState1
      .applyChanges(newState1.changes(block2).get, block2.id)
      .get

    val newWallet2 = newWallet1.scanPersistent(block2)


    assert(newState2.bfr.boxesByKey(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)
      .filter(_.isInstanceOf[ArbitBox]).length == 2)

    assert(newState2.bfr.boxesByKey(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get)
      .filter(_.isInstanceOf[ArbitBox]).length == 1)

    assert(newState2.bfr.boxesByKey(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)
      .filter(_.isInstanceOf[ArbitBox])
      .foldLeft(true) {(acc, i) => acc && (i.value == 99999995 || i.value == 4)})

    assert(newState2.bfr.boxesByKey(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get)
      .filter(_.isInstanceOf[ArbitBox]).head.value == 1)

    newState2.rollbackTo(genesisState.version)
    newWallet2.rollback(genesisState.version)


  }

  property("Rollback should have worked and recreated above changes exactly") {

    val tx1 = ArbitTransfer.create(genesisState.bfr,
      gw,
      IndexedSeq((PublicKey25519Proposition(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get), 5L)),
      IndexedSeq(PublicKey25519Proposition(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)),
      0,
      "",
    ).get

    val block1 = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(tx1), 10L, settings.version)

    require(genesisState.validate(tx1).isSuccess)

    val newState1 = genesisState
      .applyChanges(genesisState.changes(block1).get, block1.id)
      .get

    val newWallet1 = gw.scanPersistent(block1)

    assert(newState1.bfr.boxesByKey(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)
      .filter(_.isInstanceOf[ArbitBox]).length == 1)

    assert(newState1.bfr.boxesByKey(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get)
      .filter(_.isInstanceOf[ArbitBox]).length == 1)

    assert(newState1.bfr.boxesByKey(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)
      .filter(_.isInstanceOf[ArbitBox]).head.value == 99999995)

    assert(newState1.bfr.boxesByKey(Base58.decode("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb").get)
      .filter(_.isInstanceOf[ArbitBox]).head.value == 5)

  }

  override def afterAll() {
    history.storage.storage.close
  }
}
