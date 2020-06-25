package bifrost.state

import bifrost.nodeView.NodeViewHolder
import bifrost.settings.{AppSettings, StartupOpts}
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.reflect.io.Path
import scala.util.Try

class StateSpec extends PropSpec
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
        Seq(poT), 10L, settings.version)

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
      10L,
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
    import bifrost.state.BifrostStateSpec._
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
        10L,
        settings.version
      )
      genesisState.validate(poT) shouldBe a[Failure[_]]
      println()
    }
  }*/

  /*property("Attempting to validate an Arbit for amount you do not have should error") {
    import bifrost.state.BifrostStateSpec._
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
        10L,
        settings.version
      )

      genesisState.validate(arT) shouldBe a[Failure[_]]
    }
  }*/

  override def afterAll() {
    StateSpec.history.storage.storage.close()
  }
}

object StateSpec {

  import MinimalState.VersionTag
  import bifrost.nodeView.NodeViewHolder.{HIS, MP, MS, VL}

  lazy val testSettings: AppSettings = AppSettings.read(StartupOpts(Some("test.conf"), None))

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  val gs: (HIS, MS, VL, MP) = NodeViewHolder.initializeGenesis(testSettings)
  val history: HIS = gs._1
  var genesisState: MS = gs._2
  var gw: VL = gs._3

  // Unlock Secrets
  gw.unlockKeyFile("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ", "genesis")
  gw.unlockKeyFile("A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb", "genesis")
  gw.unlockKeyFile("F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU", "genesis")
  val genesisBlockId: VersionTag = genesisState.version
}