package co.topl.stakeholder

import akka.actor.{ActorPath, Props}
import com.google.common.cache.LoadingCache
import co.topl.stakeholder.primitives.{
  ActorRefWrapper,
  ByteArrayWrapper,
  Fch,
  Kes,
  KeyFile,
  Keys,
  NTPClient,
  Ratio,
  Sig,
  TetraParameters,
  Vrf
}
import co.topl.stakeholder.components.{Block, Serializer, Tine}
import co.topl.stakeholder.history.{BlockStorage, ChainStorage, StateStorage}
import co.topl.settings.AppSettings

import scala.math.BigInt
import scala.util.{Failure, Random, Success, Try}

/**
 * AMS 2020:
 * Stakeholder actor that executes the staking procedure and participates in consensus,
 * Each stakeholder actor represents a distinct node view with different modifiers in their pools and databases,
 * This is the primary node view holder in testnet configuration
 * Should only communicate with local and remote interfaces (Coordinator and Router respectively)
 * All members required for consensus are initialized
 * @param inputSeed input entropy
 * @param holderIndex an integer index for identifying executing actors in thread locks
 * @param inputRef network controller refs and router ref
 * @param inputKeyFile UI generated keyfile
 * @param inputDataDir UI chosen data path
 * @param inputPassword UI password
 * @param inputKeyDir UI key dir for updates
 */

class Stakeholder(
  inputSeed:                Array[Byte],
  override val holderIndex: Int,
  inputRef:                 Seq[ActorRefWrapper],
  inputKeyFile:             Option[KeyFile],
  inputDataDir:             Option[String],
  inputPassword:            Option[String],
  inputKeyDir:              Option[String],
  override val settings:    AppSettings
) extends ChainSelection
    with Forging
    with Ledger
    with Messages
    with Operations
    with Receive
    with Staking
    with Transactions
    with Update
    with Utilities
    with Validation {
  implicit val routerRef: ActorRefWrapper = inputRef.head
  val localRef: ActorRefWrapper = inputRef.head
  val seed: Array[Byte] = inputSeed
  val serializer: Serializer = new Serializer

  val storageDir: String = inputDataDir match {
    case None      => "coordinator/" + dataFileDir + "_" + self.path.toStringWithoutAddress.drop(5)
    case Some(dir) => dir
  }
  implicit val blocks: BlockStorage = new BlockStorage(storageDir, serializer)
  val localChain: Tine = new Tine
  val chainStorage = new ChainStorage(storageDir)
  val vrf = new Vrf
  val kes = new Kes
  val sig = new Sig
  override val fch = new Fch
  val rng: Random = new Random(BigInt(seed).toLong)
  var keys: Keys = Keys(seed, sig, vrf, kes, 0)
  val history: StateStorage = new StateStorage(storageDir, serializer)
  val holderId: ActorPath = self.path
  val sessionId: Sid = ByteArrayWrapper(fch.hash(holderId.toString))
  val phase: Double = rng.nextDouble
  val selfWrapper: ActorRefWrapper = ActorRefWrapper(self)

  //stakeholder password, set at runtime, for research runs with deterministic entropy
  var password: String = inputPassword match {
    case Some(str) => str
    case None      => ""
  }
  var derivedKey: Array[Byte] = Array()
  var salt: Array[Byte] = Array()
  var keyFile: Option[KeyFile] = inputKeyFile

  var keyDir: String = inputKeyDir match {
    case None      => storageDir + "/keys/"
    case Some(dir) => dir
  }
  var localState: State = Map()
  var eta: Eta = Array()
  var stakingState: State = Map()
  var memPool: MemPool = Map()
  //list of all or some of the stakeholders, including self, that the stakeholder is aware of
  var holders: List[ActorRefWrapper] = List()
  //map of all session IDs and public keys associated with holders in holder list
  var inbox: Map[Sid, (ActorRefWrapper, PublicKeys)] = Map()
  //slot time as determined from coordinator clock
  var globalSlot = 0
  //all tines that are pending built from new blocks that are received
  var tinePool: Map[Int, (Tine, Int, Int, Int, ActorRefWrapper)] = Map()
  //counter for identifying tines
  var tineCounter = 0
  //completed tines waiting to be selected with maxvalid-bg
  var tinePoolWithPrefix: Array[(Tine, Slot, Int)] = Array()
  //placeholder for genesis block
  var genBlockHeader: BlockHeader = _
  //placeholder for genesis block ID
  var genBlockHash: Hash = ByteArrayWrapper(Array())
  //genesis block
  var genesisBlock: Option[Block] = None
  //start system time set by coordinator
  var t0: Long = 0
  var t1: Long = 0
  //current slot that is being processed by stakeholder
  var localSlot = 0
  //current epoch that is being processed by stakeholder
  var currentEpoch = 0
  //ref of coordinator actor
  var coordinatorRef: ActorRefWrapper = _

  var bootStrapLock: Boolean = true
  var helloLock: Boolean = true
  var bootStrapJob: Int = -1
  var tineProvider: Option[ActorRefWrapper] = None
  var alphaCache: Option[LoadingCache[ByteArrayWrapper, Ratio]] = None
  var thresholdCache: Option[LoadingCache[(Ratio, Slot), Ratio]] = None
  var networkDelayList: List[Double] = List(0.0)
  var tineLengthList: List[Double] = List(0.0)

  var localClockOffset: Long = 0

  def globalTime: Long =
    System.currentTimeMillis() + localClockOffset

}

object Stakeholder {

  def props(seed: Array[Byte], index: Int, ref: Seq[akka.actor.ActorRef], settings: AppSettings): Props =
    Props(
      new Stakeholder(
        seed,
        index,
        ref.map(ActorRefWrapper(_)(ActorRefWrapper.routerRef(ref.head))),
        None,
        None,
        None,
        None,
        settings
      )
    ).withDispatcher(TetraParameters.stakeholderEC)

  def props(
    seed:     Array[Byte],
    index:    Int,
    ref:      Seq[akka.actor.ActorRef],
    keyFile:  KeyFile,
    dir:      String,
    password: String,
    kdir:     String,
    settings: AppSettings
  ): Props =
    Props(
      new Stakeholder(
        seed,
        index,
        ref.map(ActorRefWrapper(_)(ActorRefWrapper.routerRef(ref.head))),
        Some(keyFile),
        Some(dir),
        Some(password),
        Some(kdir),
        settings
      )
    ).withDispatcher(TetraParameters.stakeholderEC)
}
