package bifrost.wallet

import java.io.File
import java.security.SecureRandom

import bifrost.blocks.BifrostBlock
import bifrost.keygen.KeyFile
import bifrost.scorexMod.{GenericWalletBox, GenericWalletBoxSerializer, Wallet, WalletTransaction}
import bifrost.state.BifrostState
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.MofNProposition
import com.google.common.primitives.Ints
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.settings.Settings
import bifrost.transaction.bifrostTransaction.BifrostTransaction
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import bifrost.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}


case class BWallet(var secrets: Set[PrivateKey25519], store: LSMStore, defaultKeyDir: String)
  extends Wallet[Any, ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBlock, BWallet]
    with ScorexLogging {

  import bifrost.wallet.BWallet._

  override type S = PrivateKey25519
  override type PI = ProofOfKnowledgeProposition[S]

  private val SecretsKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(store.keySize)(2: Byte))

  private val BoxIdsKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(store.keySize)(1: Byte))

  def boxIds: Seq[Array[Byte]] = store
    .get(BoxIdsKey)
    .map(_
           .data
           .grouped(store.keySize)
           .toSeq)
    .getOrElse(Seq[Array[Byte]]())

  private lazy val walletBoxSerializer = new GenericWalletBoxSerializer[Any, PI, BifrostBox](BifrostBoxSerializer)

  //not implemented intentionally for now
  override def historyTransactions: Seq[WalletTransaction[PI, BifrostTransaction]] = ???

  //Removed filtering of 0 value boxes since they should no longer be created based on changes to newBoxes for each transaction
  override def boxes(): Seq[GenericWalletBox[Any, PI, BifrostBox]] = {
    //log.debug(s"${Console.GREEN}Accessing boxes: ${boxIds.toList.map(Base58.encode)}${Console.RESET}")
    boxIds
      .flatMap(id => store.get(ByteArrayWrapper(id)))
      .map(_.data)
      .map(ba => walletBoxSerializer.parseBytes(ba))
      .filter {
        case s: Success[GenericWalletBox[Any, PI, BifrostBox]] => true
//          s.value.box match {
//          case pb: PolyBox => pb.value > 0
//          case cb: ContractBox => true
//          case ab: ArbitBox => ab.value > 0
//          case profB: ProfileBox => ProfileBox.acceptableKeys.contains(profB.key)
//          case reputationB: ReputationBox => reputationB.value._1.isInstanceOf[Double] && reputationB.value._2
//            .isInstanceOf[Double]
//          case assetB: AssetBox => assetB.amount > 0
//        }
        case _ => false
      }
      .map(_.get)
  }

  //Only returns asset, arbit and poly boxes by public key
   def boxesByKey(publicKeyString: String): Seq[GenericWalletBox[Any, PI, BifrostBox]] = {
    //log.debug(s"${Console.GREEN}Accessing boxes: ${boxIds.toList.map(Base58.encode)}${Console.RESET}")
    boxIds
      .flatMap(id => store.get(ByteArrayWrapper(id)))
      .map(_.data)
      .map(ba => walletBoxSerializer.parseBytes(ba))
      .filter {
        case s: Success[GenericWalletBox[Any, PI, BifrostBox]] => s.value.box match {
          case pb: PolyBox =>
//            pb.value > 0 &&
            publicKeyString == Base58.encode(pb.proposition.pubKeyBytes)
          case ab: ArbitBox =>
//            ab.value > 0 &&
              publicKeyString == Base58.encode(ab.proposition.pubKeyBytes)
          case assetB: AssetBox =>
//            assetB.amount > 0 &&
              publicKeyString == Base58.encode(assetB.proposition.pubKeyBytes)
        }
        case _ => false
      }
      .map(_.get)
  }

  override def publicKeys: Set[PI] = {
    //secrets.map(_.publicImage)
    getListOfFiles(defaultKeyDir).map(file => PublicKey25519Proposition(KeyFile.readFile(file.getPath).pubKeyBytes))
      .toSet
  }

  def unlockKeyFile(publicKeyString: String, password: String): Unit = {
    val keyfiles = getListOfFiles(defaultKeyDir)
      .map(file => KeyFile.readFile(file.getPath))
      .filter(k => k
        .pubKeyBytes sameElements Base58
        .decode(publicKeyString)
        .get)

    assert(keyfiles.size == 1, "Cannot find a unique publicKey in key files")
    val privKey = keyfiles.head.getPrivateKey(password) match {
      case Success(priv) => Set(priv)
      case Failure(e) => throw e
    }
    // ensure no duplicate by comparing privKey strings
    if (!secrets.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.head.privKeyBytes))) {
      // secrets.empty // should empty the current set of secrets meaning unlock only allows a single key to be unlocked at once
      secrets += privKey.head
    } else {
      log.warn(s"$publicKeyString is already unlocked")
    }
  }

  def lockKeyFile(publicKeyString: String, password: String): Unit = {
    val keyfiles = getListOfFiles(defaultKeyDir)
      .map(file => KeyFile.readFile(file.getPath))
      .filter(k => k
        .pubKeyBytes sameElements Base58
        .decode(publicKeyString)
        .get)
    assert(keyfiles.size == 1, "Cannot find a unique publicKey in key files")
    val privKey = keyfiles.head.getPrivateKey(password) match {
      case Success(priv) => Set(priv)
      case Failure(e) => throw e
    }
    // ensure no duplicate by comparing privKey strings
    if (!secrets.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.head.privKeyBytes))) {
      log.warn(s"$publicKeyString is already locked")
    } else {
      secrets -= (secrets find (p => Base58.encode(p.privKeyBytes) == Base58.encode(privKey.head.privKeyBytes))).get
    }
  }

  override def secretByPublicImage(publicImage: PI): Option[S] = publicImage match {
    case p: PublicKey25519Proposition => secrets.find(s => s.publicImage == p)
    case mn: MofNProposition => secrets.find(s => mn.setOfPubKeyBytes.exists(s.publicImage == PublicKey25519Proposition(
      _)))
    case _ => None
  }

  def generateNewSecret(): BWallet = {
    // Avoid characters that could be easily mistaken for one another (e.g. 1 and l)
    val letters = "abcdefghjkmnpqrstuvwxyzABCDEFGHJKMNPQRSTUVWXYZ23456789!@#$%&*?"
    val secureRng = SecureRandom.getInstanceStrong
    val password = (0 until 18)
      .map(i => letters.charAt(secureRng.nextInt(letters.length)))
      .mkString

    log.warn(s"Generated Password is <<$password>>. Make sure to record this since this will never appear again!")
    val privKey = KeyFile(password, defaultKeyDir = defaultKeyDir).getPrivateKey(password).get

    BWallet(secrets + privKey, store, defaultKeyDir)
  }

  def generateNewSecret(password: String): PublicKey25519Proposition = {
    val privKey = KeyFile(password = password, defaultKeyDir = defaultKeyDir).getPrivateKey(password).get
    secrets += privKey
    privKey.publicImage
  }

  def generateNewSecret(password: String, importSeed: String): PublicKey25519Proposition = {
    val privKey = KeyFile(password,seed = FastCryptographicHash(importSeed), defaultKeyDir = defaultKeyDir).getPrivateKey(password).get
    secrets += privKey
    privKey.publicImage
  }

  def inWallet(publicImage: PI): Boolean = publicImage match {
    case p: PublicKey25519Proposition => publicKeys.contains(p)
    case mn: MofNProposition => publicKeys.exists(p => mn.setOfPubKeyBytes.exists(p == PublicKey25519Proposition(_)))
  }

  //we do not process offchain (e.g. by adding them to the wallet)
  override def scanOffchain(tx: BifrostTransaction): BWallet = this

  override def scanOffchain(txs: Seq[BifrostTransaction]): BWallet = this

  override def scanPersistent(modifier: BifrostBlock): BWallet = {
    log.debug(s"Applying modifier to wallet: ${Base58.encode(modifier.id)}")
    val changes = BifrostState.changes(modifier).get

    val newBoxes = changes
      .toAppend
      .filter(s => inWallet(s.proposition))
      .map { box =>
        val boxTransaction = modifier
          .transactions
          .getOrElse(Seq())
          .find(t => t.newBoxes.exists(tb => tb.id sameElements box.id))

        val txId = boxTransaction
          .map(_.id)
          .getOrElse(Array.fill(32)(0: Byte))

        val ts = boxTransaction
          .map(_.timestamp)
          .getOrElse(modifier.timestamp)

        val wb = GenericWalletBox[Any, PI, BifrostBox](box, txId, ts)(BifrostBoxSerializer)
        ByteArrayWrapper(box.id) -> ByteArrayWrapper(wb.bytes)
      }

    val boxIdsToRemove = (changes.boxIdsToRemove -- newBoxes.map(_._1.data)).map(ByteArrayWrapper.apply)
    val newBoxIds: ByteArrayWrapper = ByteArrayWrapper(
      newBoxes
        .filter(b => !boxIds.exists(b._1.data sameElements _))
        .toArray
        .flatMap(_._1.data) ++
        boxIds.filter(bi => {
          !boxIdsToRemove.exists(_.data sameElements bi)
        }).flatten
    )
//    log.debug(s"${Console.RED} Number of boxes in wallet ${boxIds.length}${Console.RESET}")

    store.update(ByteArrayWrapper(modifier.id), boxIdsToRemove, Seq(BoxIdsKey -> newBoxIds) ++ newBoxes)

    BWallet(secrets, store, defaultKeyDir)
  }

  override def rollback(to: VersionTag): Try[BWallet] = Try {
    if (store.lastVersionID.exists(_.data sameElements to)) {
      this
    } else {
      log.debug(s"Rolling back wallet to: ${Base58.encode(to)}")
      store.rollback(ByteArrayWrapper(to))
      BWallet(secrets, store, defaultKeyDir)
    }
  }

  override type NVCT = this.type

}

object BWallet {

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def walletFile(settings: Settings): File = {
    val walletDirOpt = settings.walletDirOpt.ensuring(_.isDefined, "wallet dir must be specified")
    val walletDir = walletDirOpt.get
    new File(walletDir).mkdirs()

    new File(s"$walletDir/wallet.dat")
  }

  def exists(settings: Settings): Boolean = walletFile(settings).exists()

  private def directoryEnsuring(dirPath: String): Boolean = {
    val f = new java.io.File(dirPath)
    f.mkdirs()
    f.exists()
  }

  def readOrGenerate(settings: Settings, seed: String): BWallet = {
    val wFile = walletFile(settings)
    wFile.mkdirs()
    val boxesStorage = new LSMStore(wFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        boxesStorage.close()
      }
    })
    // Create directory for key files
    val keyFileDir = settings
      .settingsJSON
      .get("keyFileDir")
      .flatMap(_.asString)
      .ensuring(pathOpt => pathOpt.forall(directoryEnsuring))

    BWallet(Set(), boxesStorage, keyFileDir.get)
  }

  def readOrGenerate(settings: Settings): BWallet = {
    val gw = readOrGenerate(settings, Base58.encode(settings.walletSeed))
    if (Base58.encode(settings.walletSeed).startsWith("genesis")) {
      val seeds = (0 to 2).map(c => FastCryptographicHash(settings.walletSeed ++ Ints.toByteArray(c)))
      val pubKeys = seeds.map { seed =>
        val (priv, pub) = PrivateKey25519Companion.generateKeys(seed)
        if (!gw.publicKeys.contains(pub)) {
          KeyFile("genesis", seed = seed, gw.defaultKeyDir)
        }
        pub
      }
      gw.unlockKeyFile(Base58.encode(pubKeys.head.pubKeyBytes), "genesis")
    }
    gw
  }

  def readOrGenerate(settings: Settings, seed: String, accounts: Int): BWallet =
    (1 to accounts).foldLeft(readOrGenerate(settings, seed)) { case (w, _) =>
      w.generateNewSecret()
    }

  def readOrGenerate(settings: Settings, accounts: Int): BWallet =
    (1 to accounts).foldLeft(readOrGenerate(settings)) { case (w, _) =>
      w
    }

  //wallet with applied initialBlocks
  def genesisWallet(settings: Settings, initialBlocks: Seq[BifrostBlock]): BWallet = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (a, b) =>
      a.scanPersistent(b)
    }
  }
}
