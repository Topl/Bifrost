package bifrost.state

import java.io.File
import java.time.Instant

import bifrost.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.{Ints, Longs}
import bifrost.blocks.BifrostBlock
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.state.BifrostState
import bifrost.transaction.{ArbitTransfer, ContractCreation, PolyTransfer}
import bifrost.transaction.box.{ArbitBox, ContractBox, ContractBoxSerializer, PolyBox}
import bifrost.transaction.box.proposition.MofNPropositionSerializer
import bifrost.wallet.BWallet
import io.circe
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.Try

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

  val GenesisAccountsNum = 50
  val GenesisBalance = 100000000L

  //propositions with wallet seed genesisoo, genesiso1, ..., genesis48, genesis49
  val icoMembers: IndexedSeq[PublicKey25519Proposition] =
    IndexedSeq(
      "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ", "7BDhJv6Wh2MekgJLvQ98ot9xiw5x3N4b3KipURdrW8Ge",
      "Ei8oY3eg5vM26QUBhyFiAdPN1C23RJEV9irrykNmSAFV", "8LNhm5QagL88sWggvJKGDiZ5bBCG4ajV7R6vAKz4czA9",
      "EakiCSw1rfmL5DFTPNmSJZEEAEGtTp3DN12wVPJVsURS", "AEQ8bZRuAxAp8DV9VZnTrSudGPdNyzY2HXjPBCGy8igf",
      "DSL6bvb6j1v6SnvKjqc6fJWdsRjZ85YboH8FkzonUPiT", "419sTmWKAXb5526naQ93xJZL4YAYtpVkbLmzMb6k5X9m",
      "GydWCS1GwExoDNuEiW6fBLYr7cs4vwdLpk1kzDeKHq6A", "G8xVDYow1YcSb4cuAHwcpYSEKxFpYwC9GqYChMvbCWn5",
      "9E4F53GSXMPqwuPWEVoUQe9B1z4A8v9Y6tAQdKK779km", "5XtHBDxXCudA38FJnoWm1BVG8aV67AiQKnPuwYbWZCb3",
      "8Sp3v5vtYtkM9Z2K2B7PuZbWmWQE9bfiUFCvkmsdauGj", "8XTUXeLiHPbMNXedWQh5xHQtq4xUHU3pZZGqRQzC2eyj",
      "ftqJXjSXrWQXmumNVVaRiNB7TZuCy4GCvz9V4GJGhAv", "GMAYWvbBmssCr55m9bcq8cKzfczSKKxidtVrukBM1KFN",
      "3nFprwUuqGH9BpvJMQeCb5AwHdaXuxKin1WSxWc9PTkY", "HfYNA96cGebFGgAhGUbxvRJYyLFchQJZpJTQMXztE6gZ",
      "EPbo8xRWARg2znJAqevKnQMskxnemmCdimPiVFhr8eLd", "4pygr1SPEe5KbU1R8XgMmYaW7YfTH818wd113mF6bhsP",
      "52gwahUytUXv7wfKs4j6YeKeepc38sYsUi4jp4z4jVym", "Hi3Q1ZQbD2zztq6ajm5yUKfFccxmj3yZn79GUjhFvPSW",
      "G1yK5iwPQKNXnqU4Drg83et3gKhRW5CogqiekKEYDcrt", "Hf8XcEAVMCiWbu376rGS48FhwH5NgteivfsTsvX1XpbA",
      "3FAskwxrbqiX2KGEnFPuD3z89aubJvvdxZTKHCrMFjxQ", "GgahaaNBaHRnyUtvEu3k7N5BnW3dvhVCXyxMP6uijdhh",
      "7R9waVeAKuHKNQY5uTYBp6zNLNo6wSDvj9XfQCyRWmDF", "E4AoFDANgDFL83gTS6A7kjWbLmqWcPr6DqEgMG7cqU18",
      "AEkuiLFdudYmUwZ9dSa64rakqUgJZf6pKFFwwm6CZFQz", "3QzGZvvTQbcUdhd5BL9ofEK3GdzbmqUnYA1pYTAdVY44",
      "EjpGvdZETt3SuZpcuwKvZS4jgWCockDHzFQLoeYNW4R", "C85c1uMAiHKDgcqxF6EaiCGQyWgQEYATbpo8M7XEnx3R",
      "8V5y1CSC1gCGD1jai3ns5FJNW7tAzf7BGd4iwmBv7V44", "CJ9udTDT61ckSHMd6YNpjeNdsN2fGwmJ6Ry6YERXmGa7",
      "7eboeRCeeBCFwtzPtB4vKPnaYMPL52BjfiEpqSRWfkgx", "E3JJCTMouTys5BSwFyHTV3Ht55mYWfNUAverrNaVo4jE",
      "9PLHPwnHyA5jf6GPGRjJt7HNd93rw4gWTBi7LBNL4Wwt", "2YM2FQ4HfMiV3LFkiwop2xFznbPVEHbhahVvcrhfZtXq",
      "3oTzYXjwdr684FUzaJEVVuXBztysNgR8M8iV9QykaM9C", "J6bgGpwDMqKFrde2mpdS6dasRyn9WFV6jKgWAkHSN91q",
      "4wtQpa1BVgAt9CA4FUuHZHCYGBYtvudPqa1sAddfAPii", "DaSXwzkAU2WfH39zxMfuXpExsVfKk6JzeYbdW9RLiXr4",
      "6BtXEZE6GcxtEtSLAHXkE3mkcTG1u8WuoQxZG7R8BR5X", "39Z9VaCAeqoWajHyku29argf7zmVqs2vVJM8zYe7YLXy",
      "7focbpSdsNNE4x9h7eyXSkvXE6dtxsoVyZMpTpuThLoH", "CBdnTL6C4A7nsacxCP3VL3TqUokEraFy49ckQ196KU46",
      "CfvbDC8dxGeLXzYhDpNpCF2Ar9Q5LKs8QrfcMYAV59Lt", "GFseSi5squ8GRRkj6RknbGj9Hyz82HxKkcn8NKW1e5CF",
      "FuTHJNKaPTneEYRkjKAC3MkSttvAC7NtBeb2uNGS8mg3", "5hhPGEFCZM2HL6DNKs8KvUZAH3wC47rvMXBGftw9CCA5"
    ).map(s => PublicKey25519Proposition(Base58.decode(s).get))

  val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)
  val genesisAccountPriv = genesisAccount._1

  val genesisTxs = Seq(ArbitTransfer(
    IndexedSeq(genesisAccountPriv -> 0),
    icoMembers.map(_ -> GenesisBalance),
    0L,
    0L)) ++ Seq(PolyTransfer(
    IndexedSeq(genesisAccountPriv -> 0),
    icoMembers.map(_ -> GenesisBalance),
    0L,
    0L))
  assert(icoMembers.length == GenesisAccountsNum)
  assert(Base58.encode(genesisTxs.head.id) == "5dJRukdd7sw7cmc8vwSnwbVggWLPV4VHYsZt7AQcFW3B", Base58.encode(genesisTxs.head.id))

  val genesisBox = ArbitBox(genesisAccountPriv.publicImage, 0, GenesisBalance)
  val genesisBlock = BifrostBlock.create(settings.GenesisParentId, 0L, genesisTxs, genesisBox, genesisAccountPriv)

  var history = BifrostHistory.readOrGenerate(settings)
  history = history.append(genesisBlock).get._1


  var genesisState = BifrostState.genesisState(settings, Seq(genesisBlock))
  genesisState = genesisState
  val genesisBlockId = genesisState.version
  val gw = BWallet.genesisWallet(settings, Seq(genesisBlock))
  assert(!Base58.encode(settings.walletSeed).startsWith("genesis") || gw.boxes().flatMap(_.box match {
    case ab: ArbitBox => Some(ab.value)
    case _ => None
  }).sum >= GenesisBalance)

  gw.boxes().foreach(b => assert(genesisState.closedBox(b.box.id).isDefined))

  property("A block with valid contract creation will result in an entry in the LSMStore") {

    // Create block with contract creation
    forAll(contractCreationGen) {
      cc: ContractCreation =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now().toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cc)
        )

        val box = cc.newBoxes.head.asInstanceOf[ContractBox]

        val boxBytes = ContractBoxSerializer.toBytes(box)

        val newState = genesisState.applyChanges(genesisState.changes(block).get, Ints.toByteArray(1)).get

        require(newState.storage.get(ByteArrayWrapper(box.id)) match {
          case Some(wrapper) => wrapper.data sameElements boxBytes
          case None => false
        })

        genesisState = newState.rollbackTo(genesisBlockId).get

    }
  }

  property("A block with valid PolyTransfer should result in more funds for receiver, less for transferrer") {
    // Create genesis block, add to state
    // Create new block with PolyTransfer
    // send new block to state
    // check updated state
    forAll(validPolyTransferGen) { PoT: PolyTransfer =>
      val block = BifrostBlock(
        Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
        Instant.now().toEpochMilli,
        ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
        Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
        Seq(PoT)
      )

      val newState = genesisState.applyChanges(genesisState.changes(block).get, Ints.toByteArray(2)).get

//      val arbitBoxes = gw.boxes().filter(_.box match {
//        case a: ArbitBox => newState.closedBox(a.id).isDefined
//        case _ => false
//      }).map(_.box.asInstanceOf[ArbitBox])
//
//      val boxKeys = arbitBoxes.flatMap(b => gw.secretByPublicImage(b.proposition).map(s => (b, s)))
//      println(s"Arbit Balance ${boxKeys.map(_._1.value).sum}")

      genesisState = newState.rollbackTo(genesisBlockId).get
    }
  }

  property("Attempting to validate a contract creation tx without valid signatures should error") {
    // Create invalid contract creation
    // send tx to state
  }

  property("Attempting to validate a contract creation tx with a timestamp that is before the last block timestamp should error") {

  }

  property("Attempting to validate a contract creation tx with a timestamp that is in the future should error") {

  }

  property("Attempting to validate a contract creation tx with the same signature as an existing contract should error") {
    // Create invalid contract creation
    // send tx to state
  }

  property("Attempting to validate a contract creation tx with a timestamp too far in the future should error") {
    // Create invalid contract creation
    // send tx to state
  }

  property("Attempting to validate a PolyTransfer for amount you do not have should error") {
    // Create invalid PolyTransfer
    // send tx to state
  }

  override def afterAll() {
    history.storage.storage.close()
    val path: Path = Path ("/tmp")
    Try(path.deleteRecursively())
  }
}