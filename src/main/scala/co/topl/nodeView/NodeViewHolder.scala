package co.topl.nodeView

import akka.actor.{ ActorRef, ActorSystem, Props }
import co.topl.crypto.{ PrivateKey25519, Signature25519 }
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{ Block, BlockSerializer }
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.modifier.transaction.{ ArbitTransfer, GenericTransaction, PolyTransfer, Transaction }
import co.topl.nodeView.NodeViewModifier.ModifierTypeId
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{ ArbitBox, Box }
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{ AppSettings, AppContext }
import co.topl.utils.serialization.BifrostSerializer
import co.topl.utils.{ Logging, TimeProvider }
import co.topl.wallet.Wallet
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext

class NodeViewHolder ( override val settings: AppSettings, appContext: AppContext )
                     ( implicit ec: ExecutionContext )
  extends GenericNodeViewHolder[NodeViewHolder.BX, NodeViewHolder.TX, NodeViewHolder.PMOD, NodeViewHolder.HIS,
                                NodeViewHolder.MS, NodeViewHolder.VL, NodeViewHolder.MP] {

  lazy val modifierCompanions: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]] =
    Map(Block.modifierTypeId -> BlockSerializer,
        GenericTransaction.modifierTypeId -> TransactionSerializer)

  private val timeProvider: TimeProvider = appContext.timeProvider

  override def preRestart ( reason: Throwable, message: Option[Any] ): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }

  override def postStop ( ): Unit = {
    log.info(s"${Console.RED}Application is going down NOW!${Console.RESET}")
    super.postStop()
    nodeView._1.closeStorage() // close History storage
    nodeView._2.closeStorage() // close State storage
  }

  /**
   * Restore a local view during a node startup. If no any stored view found
   * (e.g. if it is a first launch of a node) None is to be returned
   */
  override def restoreState ( ): Option[NodeView] = {
    if ( Wallet.exists(settings) ) {
      Some((
             History.readOrGenerate(settings),
             State.readOrGenerate(settings),
             Wallet.readOrGenerate(settings, 1),
             MemPool.emptyPool
           ))
    } else None
  }

  //noinspection ScalaStyle
  override protected def genesisState: NodeView = {
    NodeViewHolder.initializeGenesis(settings)
  }
}

object NodeViewHolder extends Logging {

  type HIS = History
  type MS = State
  type VL = Wallet
  type MP = MemPool
  type PMOD = Block
  type TX = Transaction
  type BX = Box
  type NodeView = (HIS, MS, VL, MP)

  //noinspection ScalaStyle
  def initializeGenesis ( settings: AppSettings ): NodeView = {
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

    val genesisAccount = PrivateKey25519.generateKeys("genesis".getBytes)
    val genesisAccountPriv = genesisAccount._1

    val arbTx = ArbitTransfer(IndexedSeq(genesisAccountPriv.publicImage -> 0L),
                              icoMembers.map(_ -> GenesisBalance),
                              Map(genesisAccountPriv.publicImage -> Signature25519(Array.fill(Signature25519.SignatureSize)(1: Byte))),
                              0L,
                              0L,
                              "")
    val polyTx = PolyTransfer(IndexedSeq(genesisAccountPriv.publicImage -> 0L),
                              icoMembers.map(_ -> GenesisBalance),
                              Map(genesisAccountPriv.publicImage -> Signature25519(Array.fill(Signature25519.SignatureSize)(1: Byte))),
                              0L,
                              0L,
                              "")
    val genesisTxs = Seq(arbTx, polyTx)

    log.debug(s"Initialize state with transaction ${genesisTxs.head} with boxes ${genesisTxs.head.newBoxes}")
    assert(icoMembers.length == GenesisAccountsNum)

    val genesisBox = ArbitBox(genesisAccountPriv.publicImage, 0, GenesisBalance)

    val genesisBlock = Block.create(ModifierId(History.GenesisParentId), 0L, genesisTxs, genesisBox, genesisAccountPriv, settings.forgingSettings.version)

    assert(genesisBlock.encodedId == "9VX9smBd7Jz56HzTcmY6EZiLfrn7WdxECbsSgNRrPXmu", s"${Console.RED}MALFORMED GENESIS BLOCK! The calculated genesis block " +
      s"with id ${genesisBlock.encodedId} does not match the required block for the chosen network mode.${Console.RESET}")

    val history = History.readOrGenerate(settings).append(genesisBlock).get._1
    val state = State.genesisState(settings, Seq(genesisBlock))
    val wallet = Wallet.genesisWallet(settings, Seq(genesisBlock))

    assert(!settings.walletSeed.startsWith("genesis") || wallet.boxes().flatMap(_.box match {
                                                                                case ab: ArbitBox => Some(ab.value)
                                                                                case _            => None
                                                                              }).sum >= GenesisBalance)

    wallet.boxes().foreach(b => assert(state.getBox(b.box.id).isDefined))

    (history, state, wallet, MemPool.emptyPool)
  }
}

object NodeViewHolderRef {

  def apply ( settings: AppSettings, appContext: AppContext )
            ( implicit system: ActorSystem, ec: ExecutionContext ): ActorRef =
    system.actorOf(props(settings, appContext))

  def apply ( name: String, settings: AppSettings, appContext: AppContext )
            ( implicit system: ActorSystem, ec: ExecutionContext ): ActorRef =
    system.actorOf(props(settings, appContext), name)

  def props ( settings: AppSettings, appContext: AppContext )
            ( implicit ec: ExecutionContext ): Props =
    Props(new NodeViewHolder(settings, appContext))
}