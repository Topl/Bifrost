package bifrost

import bifrost.blocks.{BifrostBlock, BifrostBlockCompanion}
import bifrost.forging.ForgingSettings
import bifrost.history.{BifrostHistory, BifrostSyncInfo}
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder
import bifrost.state.BifrostState
import bifrost.transaction.box.{ArbitBox, BifrostBox}
import bifrost.wallet.BWallet
import bifrost.NodeViewModifier
import bifrost.NodeViewModifier.ModifierTypeId
import bifrost.serialization.Serializer
import bifrost.transaction.Transaction
import bifrost.transaction.bifrostTransaction.{ArbitTransfer, BifrostTransaction, PolyTransfer}
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.serialization.BifrostTransactionCompanion
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import bifrost.utils.ScorexLogging
import scorex.crypto.encode.Base58

class BifrostNodeViewHolder(settings: ForgingSettings)
  extends GenericNodeViewHolder[Any, ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBox, BifrostBlock] {

  override val networkChunkSize: Int = settings.networkChunkSize
  override type SI = BifrostSyncInfo
  override type HIS = BifrostHistory
  override type MS = BifrostState
  override type VL = BWallet
  override type MP = BifrostMemPool

  override lazy val modifierCompanions: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] =
    Map(BifrostBlock.ModifierTypeId -> BifrostBlockCompanion,
    Transaction.ModifierTypeId -> BifrostTransactionCompanion)

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  override def restoreState(): Option[NodeView] = {
    if (BWallet.exists(settings)) {
      val x = BifrostHistory.readOrGenerate(settings)
      Some(
        (
          x,
          BifrostState.readOrGenerate(settings, true, x),
          BWallet.readOrGenerate(settings, 1),
          BifrostMemPool.emptyPool
        )
      )
    } else None
  }

  //noinspection ScalaStyle
  override protected def genesisState: NodeView = {
    BifrostNodeViewHolder.initializeGenesis(settings)
  }
}

object BifrostNodeViewHolder extends ScorexLogging {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool

  type NodeView = (HIS, MS, VL, MP)

  //noinspection ScalaStyle
  def initializeGenesis(settings: ForgingSettings): NodeView = {
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
      0L,
      "")) ++ Seq(PolyTransfer(
      IndexedSeq(genesisAccountPriv -> 0),
      icoMembers.map(_ -> GenesisBalance),
      0L,
      0L,
      ""))
    log.debug(s"Initialize state with transaction ${genesisTxs.head} with boxes ${genesisTxs.head.newBoxes}")
    assert(icoMembers.length == GenesisAccountsNum)
    //YT - commented out below assertion since transaction id generation was changed
    //assert(Base58.encode(genesisTxs.head.id) == "5dJRukdd7sw7cmc8vwSnwbVggWLPV4VHYsZt7AQcFW3B", Base58.encode(genesisTxs.head.id))

    val genesisBox = ArbitBox(genesisAccountPriv.publicImage, 0, GenesisBalance)

    val genesisBlock = BifrostBlock.create(settings.GenesisParentId, 0L, genesisTxs, genesisBox, genesisAccountPriv, 10L, settings.version) // arbitrary inflation for first block of 10 Arbits

    var history = BifrostHistory.readOrGenerate(settings)
    history = history.append(genesisBlock).get._1

    val gs = BifrostState.genesisState(settings, Seq(genesisBlock), history)
    val gw = BWallet.genesisWallet(settings, Seq(genesisBlock))

    assert(!Base58.encode(settings.walletSeed).startsWith("genesis") || gw.boxes().flatMap(_.box match {
      case ab: ArbitBox => Some(ab.value)
      case _ => None
    }).sum >= GenesisBalance)

    gw.boxes().foreach(b => assert(gs.closedBox(b.box.id).isDefined))

    (history, gs, gw, BifrostMemPool.emptyPool)
  }
}