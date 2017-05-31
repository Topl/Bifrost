package bifrost.wallet

import java.io.File

import com.google.common.primitives.Ints
import bifrost.blocks.BifrostBlock
import bifrost.scorexMod.{GenericWalletBox, GenericWalletBoxSerializer, Wallet, WalletTransaction}
import bifrost.state.BifrostState
import bifrost.transaction.BifrostTransaction
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.MofNProposition
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion, PrivateKey25519Serializer}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}


case class BWallet(seed: Array[Byte], store: LSMStore)
  extends Wallet[Any, ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBlock, BWallet]
    with ScorexLogging {

  override type S = PrivateKey25519
  override type PI = ProofOfKnowledgeProposition[S]

  private val SecretsKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(store.keySize)(2: Byte))

  private val BoxIdsKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(store.keySize)(1: Byte))

  def boxIds: Seq[Array[Byte]] = {
    store.get(BoxIdsKey).map(_.data.grouped(store.keySize).toSeq).getOrElse(Seq[Array[Byte]]())
  }

  private lazy val walletBoxSerializer = new GenericWalletBoxSerializer[Any, PI, BifrostBox](BifrostBoxSerializer)

  //not implemented intentionally for now
  override def historyTransactions: Seq[WalletTransaction[PI, BifrostTransaction]] = ???

  override def boxes(): Seq[GenericWalletBox[Any, PI, BifrostBox]] = {
    println(s"${Console.GREEN}Accessing boxes: ${boxIds.toList.map(Base58.encode)}${Console.RESET}")
    boxIds
      .flatMap(id => store.get(ByteArrayWrapper(id)))
      .map(_.data)
      .map(ba => walletBoxSerializer.parseBytes(ba))
      .filter {
        case s: Success[GenericWalletBox[Any, PI, BifrostBox]] => s.value.box match {
          case pb: PolyBox => pb.value > 0
          case cb: ContractBox => true
          case ab: ArbitBox => ab.value > 0
          case profB: ProfileBox => ProfileBox.acceptableKeys.contains(profB.key)
        }
        case _ => false
      }
      .map(_.get)
  }

  override def publicKeys: Set[PI] = secrets.map(_.publicImage)

  override def secrets: Set[S] = store.get(SecretsKey)
    .map(_.data.grouped(64).map(b => PrivateKey25519Serializer.parseBytes(b).get).toSet)
    .getOrElse(Set.empty[PrivateKey25519])

  override def secretByPublicImage(publicImage: PI): Option[S] = publicImage match {
    case p: PublicKey25519Proposition => secrets.find(s => s.publicImage == p)
    case mn: MofNProposition => secrets.find(s => mn.setOfPubKeyBytes.exists(s.publicImage == PublicKey25519Proposition(_)))
    case _ => None
  }

  override def generateNewSecret(): BWallet = {
    val prevSecrets = secrets
    val nonce: Array[Byte] = Ints.toByteArray(prevSecrets.size)
    val s = FastCryptographicHash(seed ++ nonce)
    val (priv, _) = PrivateKey25519Companion.generateKeys(s)
    val allSecrets: Set[PrivateKey25519] = Set(priv) ++ prevSecrets
    store.update(ByteArrayWrapper(priv.privKeyBytes),
      Seq(),
      Seq(SecretsKey -> ByteArrayWrapper(allSecrets.toArray.flatMap(p => PrivateKey25519Serializer.toBytes(p)))))
    BWallet(seed, store)
  }

  //we do not process offchain (e.g. by adding them to the wallet)
  override def scanOffchain(tx: BifrostTransaction): BWallet = this

  override def scanOffchain(txs: Seq[BifrostTransaction]): BWallet = this

  override def scanPersistent(modifier: BifrostBlock): BWallet = {
    log.debug(s"Applying modifier to wallet: ${Base58.encode(modifier.id)}")
    val changes = BifrostState.changes(modifier).get

    val newBoxes = changes.toAppend.filter(s => secretByPublicImage(s.proposition).isDefined).map { box =>
      val boxTransaction = modifier.transactions.getOrElse(Seq())
        .find(t => t.newBoxes.exists(tb => tb.id sameElements box.id))
      val txId = boxTransaction.map(_.id).getOrElse(Array.fill(32)(0: Byte))
      val ts = boxTransaction.map(_.timestamp).getOrElse(modifier.timestamp)
      val wb = GenericWalletBox[Any, PI, BifrostBox](box, txId, ts)(BifrostBoxSerializer)
      ByteArrayWrapper(box.id) -> ByteArrayWrapper(wb.bytes)
    }

    val boxIdsToRemove = changes.boxIdsToRemove.map(ByteArrayWrapper.apply)
    val newBoxIds: ByteArrayWrapper = ByteArrayWrapper(newBoxes.toArray.flatMap(_._1.data) ++
      boxIds.filter(bi => !boxIdsToRemove.exists(_.data sameElements bi)).flatten)
    store.update(ByteArrayWrapper(modifier.id), boxIdsToRemove, Seq(BoxIdsKey -> newBoxIds) ++ newBoxes)

    boxIds.foreach(box => println(s"Box id ${Base58.encode(box)}"))

    BWallet(seed, store)
  }

  override def rollback(to: VersionTag): Try[BWallet] = Try {
    if (store.lastVersionID.exists(_.data sameElements to)) {
      this
    } else {
      log.debug(s"Rolling back wallet to: ${Base58.encode(to)}")
      store.rollback(ByteArrayWrapper(to))
      BWallet(seed, store)
    }
  }

  override type NVCT = this.type

}

object BWallet {

  def walletFile(settings: Settings): File = {
    val walletDirOpt = settings.walletDirOpt.ensuring(_.isDefined, "wallet dir must be specified")
    val walletDir = walletDirOpt.get
    new File(walletDir).mkdirs()

    new File(s"$walletDir/wallet.dat")
  }

  def exists(settings: Settings): Boolean = walletFile(settings).exists()

  def readOrGenerate(settings: Settings, seed: String): BWallet = {
    val wFile = walletFile(settings)
    wFile.mkdirs()
    val boxesStorage = new LSMStore(wFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        boxesStorage.close()
      }
    })

    BWallet(Base58.decode(seed).get, boxesStorage)
  }

  def readOrGenerate(settings: Settings): BWallet = {
    readOrGenerate(settings, Base58.encode(settings.walletSeed))
  }

  def readOrGenerate(settings: Settings, seed: String, accounts: Int): BWallet =
    (1 to accounts).foldLeft(readOrGenerate(settings, seed)) { case (w, _) =>
      w.generateNewSecret()
    }

  def readOrGenerate(settings: Settings, accounts: Int): BWallet =
    (1 to accounts).foldLeft(readOrGenerate(settings)) { case (w, _) =>
      w.generateNewSecret()
    }

  //wallet with applied initialBlocks
  def genesisWallet(settings: Settings, initialBlocks: Seq[BifrostBlock]): BWallet = {
    initialBlocks.foldLeft(readOrGenerate(settings).generateNewSecret()) { (a, b) =>
      a.scanPersistent(b)
    }
  }
}
