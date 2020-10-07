package co.topl.nodeView.state

import co.topl.nodeView.NodeViewHolder
import co.topl.settings.{AppSettings, StartupOpts}
import co.topl.{BifrostGenerators, ValidGenerators}
import org.scalatest.BeforeAndAfterAll

import scala.reflect.io.Path
import scala.util.Try
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class StateSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators
  with BeforeAndAfterAll {

  val initialBalance = 100000000L
  //noinspection ScalaStyle
  /*property("A block with valid PolyTransfer should result in more funds for receiver, less for transferrer") {
    // Create genesis block, add to state
    // Create new block with PolyTransfer
    // send new block to state
    // check updated state
    val beforePolyBoxes = BifrostStateSpec
      .gw
      .boxes()
      .filter(_.box match {
                case a: PolyBox =>
                  BifrostStateSpec
                    .genesisState
                    .closedBox(a.id)
                    .isDefined
                case _ => false
              })
      .map(_.box.asInstanceOf[PolyBox])

    val beforeBoxKeys = beforePolyBoxes
      .flatMap(b => BifrostStateSpec
        .gw
        .secretByPublicImage(b.proposition)
        .map(s => (b, s)))

    assert(beforeBoxKeys.map(_._1.value).sum == initialBalance)

    forAll(Gen.choose(0, 500)) { _: Int =>
      val poT = PolyTransferGenerator
        .generateStatic(BifrostStateSpec.gw)
        .get

      val block = Block(
        Array.fill(Block.signatureLength)(-1: Byte),
        Instant.now().toEpochMilli,
        ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
        Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
        Seq(poT), settings.version)

      require(BifrostStateSpec.genesisState.validate(poT).isSuccess)

      val newState = BifrostStateSpec
        .genesisState
        .applyChanges(BifrostStateSpec.genesisState.changes(block).get, Ints.toByteArray(1))
        .get

      val newWallet = BifrostStateSpec.gw.scanPersistent(block)

      val arbitBoxes = newWallet
        .boxes()
        .filter(_.box match {
                  case a: ArbitBox => newState.closedBox(a.id).isDefined
                  case _ => false
                })
        .map(_.box.asInstanceOf[ArbitBox])

      val boxKeys = arbitBoxes
        .flatMap(b => newWallet
          .secretByPublicImage(b.proposition)
          .map(s => (b, s)))

      require(boxKeys.map(_._1.value).sum == initialBalance)

      val polyBoxes = newWallet
        .boxes()
        .filter(_.box match {
                  case a: PolyBox => newState.closedBox(a.id).isDefined
                  case _ => false
                })
        .map(_.box.asInstanceOf[PolyBox])

      val polyBoxKeys = polyBoxes
        .flatMap(b => newWallet
          .secretByPublicImage(b.proposition)
          .map(s => (b, s)))

      // The resulting poly balance = genesis amount - fee, because the transaction is sent to self
      require(polyBoxKeys.map(_._1.value).sum == initialBalance - poT.fee)

      BifrostStateSpec.genesisState = newState
        .rollbackTo(BifrostStateSpec.genesisBlockId)
        .get

      BifrostStateSpec.gw = newWallet
        .rollback(BifrostStateSpec.genesisBlockId)
        .get
    }
  }*/

  /*property("A block with valid ProfileTransaction should result in a ProfileBox") {
    val timestamp = System.currentTimeMillis()
    val role = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub)).head
    val privateKey = BifrostStateSpec.gw.secrets.head
    val messageToSign = ProfileTransaction
      .messageToSign(timestamp, privateKey.publicImage, Map("role" -> role.toString))

    val signature = PrivateKey25519Companion.sign(privateKey, messageToSign)
    val tx = ProfileTransaction(privateKey.publicImage, signature, Map("role" -> role.toString), 0L, timestamp)

    val block = Block(
      Array.fill(Block.signatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
      Seq(tx),
      settings.version)

    require(BifrostStateSpec.genesisState.validate(tx).isSuccess)

    val newState = BifrostStateSpec
      .genesisState
      .applyChanges(BifrostStateSpec.genesisState.changes(block).get, Ints.toByteArray(2))
      .get

    val box = newState
      .closedBox(FastCryptographicHash(privateKey.publicKeyBytes ++ "role".getBytes))
      .get
      .asInstanceOf[ProfileBox]

    BifrostStateSpec.genesisState = newState
      .rollbackTo(BifrostStateSpec.genesisBlockId)
      .get

    box.key shouldBe "role"
    box.value shouldBe role.toString
  }*/

  /*property("Attempting to validate a PolyTransfer for amount you do not have should error") {
    import co.topl.nodeView.state.BifrostStateSpec._
    val beforePolyBoxes = gw
      .boxes()
      .filter(_.box match {
                case a: PolyBox => genesisState.closedBox(a.id).isDefined
                case _ => false
              })
      .map(_.box.asInstanceOf[PolyBox])

    val beforeBoxKeys = beforePolyBoxes
      .flatMap(b => gw
        .secretByPublicImage(b.proposition)
        .map(s => (b, s)))

    assert(beforeBoxKeys.map(_._1.value).sum == initialBalance)

    //noinspection ScalaStyle
    forAll(Gen.choose(0, 100)) { _: Int =>
      val pubkeys: IndexedSeq[PublicKey25519Proposition] = gw
        .publicKeys
        .flatMap {
          case pkp: PublicKey25519Proposition => Some(pkp)
          case _ => None
        }
        .toIndexedSeq
      val toReceive = pubkeys.map(_ -> (Gen.choose(0, 100L).sample.get + initialBalance))
      val recipient = pubkeys(Random.nextInt(pubkeys.size))
      val poT = PolyTransfer.create(gw, toReceive, Random.nextInt(100),"").get
      val block = Block(
        Array.fill(Block.signatureLength)(-1: Byte),
        Instant.now().toEpochMilli,
        ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
        Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
        Seq(poT),
        settings.version
      )
      genesisState.validate(poT) shouldBe a[Failure[_]]
      println()
    }
  }*/

  /*property("Attempting to validate an Arbit for amount you do not have should error") {
    import co.topl.nodeView.state.BifrostStateSpec._
    val beforeArbitBoxes = gw
      .boxes()
      .filter(_.box match {
                case a: ArbitBox => genesisState.closedBox(a.id).isDefined
                case _ => false
              })
      .map(_.box.asInstanceOf[ArbitBox])

    val beforeBoxKeys = beforeArbitBoxes
      .flatMap(b => gw
        .secretByPublicImage(b.proposition)
        .map(s => (b, s)))

    assert(beforeBoxKeys.map(_._1.value).sum == initialBalance)

    //noinspection ScalaStyle
    forAll(Gen.choose(0, 100)) { num: Int =>
      val pubkeys: IndexedSeq[PublicKey25519Proposition] = gw
        .publicKeys
        .flatMap {
          case pkp: PublicKey25519Proposition => Some(pkp)
          case _ => None
        }
        .toIndexedSeq

      val toReceive = pubkeys.map(_ -> (Gen.choose(0, 100L).sample.get + initialBalance))
      val arT = ArbitTransfer.create(gw, toReceive, Random.nextInt(100),"").get
      val block = Block(
        Array.fill(Block.signatureLength)(-1: Byte),
        Instant.now().toEpochMilli,
        ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
        Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
        Seq(arT),
        settings.version
      )

      genesisState.validate(arT) shouldBe a[Failure[_]]
    }
  }*/

  override def afterAll() {
    StateSpec._history.closeStorage()
    StateSpec._genesisState.closeStorage()
  }
}

object StateSpec {

  import MinimalState.VersionTag
  import co.topl.nodeView.NodeViewHolder.{HIS, MP, MS, VL}

  private val settingsFilename = "src/test/resources/test.conf"
  lazy val testSettings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  val gs: (HIS, MS, VL, MP) = NodeViewHolder.initializeGenesis(testSettings)
  private val _history: HIS = gs._1
  private val _genesisState: MS = gs._2
  private val _gw: VL = gs._3

  def genesisState(): MS = gs._2.copy()

  // Unlock Secrets
  _gw.unlockKeyFile("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ", "genesis")
  _gw.unlockKeyFile("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb", "genesis")
  _gw.unlockKeyFile("F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU", "genesis")
  val genesisBlockId: VersionTag = gs._2.version
}