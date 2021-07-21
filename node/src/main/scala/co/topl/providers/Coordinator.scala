package co.topl.providers

import java.io.{BufferedWriter, File, FileWriter}
import java.time.Instant
import java.time.temporal.ChronoUnit

import akka.actor.{ActorPath, PoisonPill, Props}
import com.google.common.cache.LoadingCache
import io.iohk.iodb.ByteArrayWrapper
import co.topl.cases._
import co.topl.components
import co.topl.components.Serializer.DeserializeGenesisBlock
import co.topl.components._
import co.topl.history.{BlockStorage, ChainStorage, StateStorage, WalletStorage}
import co.topl.primitives._
import co.topl.stakeholder._
import co.topl.primitives.Base58

import scala.math.BigInt
import scala.reflect.io.Path
import scala.sys.process._
import scala.util.{Failure, Random, Success, Try}


/**
  * AMS 2020:
  * Coordinator actor that initializes the genesis block and instantiates the staking party,
  * Sends messages to participants to execute a round,
  * Acts as local interface for GUI, the global clock, and any global functionality, e.g. the genesis block,
  * Has consensus members for research oriented tests and commands
  * Acts as local interface, Should only communicate with Router and Stakeholders
  * F_INIT and G_CLOCK functionalities
  */

class Coordinator(inputSeed:Array[Byte],inputRef:Seq[ActorRefWrapper])
  extends ChainSelection
    with Forging
    with Ledger
    with Messages
    with Operations
    with Receive
    with Staking
    with Transactions
    with Update
    with Utilities
    with Validation
{
  implicit val routerRef:ActorRefWrapper = inputRef.head
  val localRef:ActorRefWrapper = inputRef(1)
  override val holderIndex: Slot = -1
  val seed:Array[Byte] = inputSeed
  val serializer:Serializer = new Serializer
  val storageDir:String = "coordinator/"+dataFileDir+"_"+simLabel+self.path.toStringWithoutAddress.drop(5)
  implicit val blocks:BlockStorage = new BlockStorage(storageDir,serializer)
  val localChain:Tine = new Tine
  val chainStorage = new ChainStorage(storageDir)
  val walletStorage = new WalletStorage(storageDir)
  val vrf = new Vrf
  val kes = new Kes
  val sig = new Sig
  val fch = new Fch
  val history:StateStorage = new StateStorage(storageDir,serializer)
  val rng:Random = new Random(BigInt(seed).toLong)
  val holderId:ActorPath = self.path
  val sessionId:Sid = ByteArrayWrapper(fch.hash(seed))
  val phase:Double = rng.nextDouble
  val selfWrapper:ActorRefWrapper = ActorRefWrapper(self)
  var keyFile:Option[KeyFile] = None
  var keyDir = ""
  var password = ""
  var derivedKey:Array[Byte] = Array()
  var salt:Array[Byte] = Array()
  var keys:Keys = Keys(seed,sig,vrf,kes,0)
  var wallet:Wallet = components.Wallet(keys.pkw)
  var chainUpdateLock = false
  var localState:State = Map()
  var eta:Eta = Array()
  var stakingState:State = Map()
  var memPool:MemPool = Map()
  var holders: List[ActorRefWrapper] = List()
  var gOff = 0
  var numHello = 0
  var inbox:Map[Sid,(ActorRefWrapper,PublicKeys)] = Map()
  var blocksForged = 0
  var globalSlot = 0
  var tinePool:Map[Int,(Tine,Int,Int,Int,ActorRefWrapper)] = Map()
  var tineCounter = 0
  var tinePoolWithPrefix:Array[(Tine,Slot,Int)] = Array()
  var genBlockHeader: BlockHeader = _
  var genBlockHash: Hash = ByteArrayWrapper(Array())
  var roundBlock: Int = 0
  var t0:Long = 0
  var t1:Long = 0
  var localSlot = 0
  var currentEpoch: Slot = -1
  var updating = false
  var actorStalled = false
  var coordinatorRef:ActorRefWrapper = _
  var txCounter = 0
  var adversary:Boolean = false
  var covert:Boolean = false
  var forgeAll:Boolean = false
  var bootStrapLock:Boolean = false
  var helloLock:Boolean = false
  var bootStrapJob:Int = 0
  var tineProvider: Option[ActorRefWrapper] = None
  var alphaCache: Option[LoadingCache[ByteArrayWrapper, Ratio]] = None
  var thresholdCache: Option[LoadingCache[(Ratio,Slot), Ratio]] = None

  val genBlockKey: Sid = ByteArrayWrapper(fch.hash("GENESIS"))

  val coordId:String = Base58.encode(inputSeed)
  val eta0:Eta = fch.hash(Base58.encode(inputSeed)+"ETA")
  val (sk_sig,pk_sig) = sig.createKeyPair(seed)
  val (sk_vrf,pk_vrf) = vrf.vrfKeypair(seed)
  val sk_kes:ForgingKey = ForgingKey(kes,seed,0)
  val pk_kes:PublicKey = sk_kes.getPublic(kes)


  var roundDone = true
  var parties: List[List[ActorRefWrapper]] = List()
  var t:Slot = 0
  var actorPaused = false
  var cmdQueue:Map[Slot,List[String]] = Parameters.inputCommands

  var fileWriter:Any = 0
  var graphWriter:Any = 0
  var transactionCounter:Int = 0
  var localClockOffset:Long = 0
  var networkDelayList: List[Double] = List(0.0)
  var tineLengthList: List[Double] = List(0.0)
  var holdersToIssueRandomly:List[ActorRefWrapper] = List()
  var genesisBlock:Option[Block] = None

  syncGlobalClock()

  println("*****************************************************************")
  println("Coordinator Seed: ["+inputSeedString+"]")
  println("*****************************************************************")
  def readFile(file:File): Seq[String] = {
    val bufferedSource = scala.io.Source.fromFile(file)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }

  def globalTime:Long = {
    System.currentTimeMillis()+localClockOffset
  }

  def syncGlobalClock(): Unit = {
    val timeDataFile = new File(s"$storageDir/time/timeInfo")
    val timeGenesis : Option[Array[String]] = Try{
      scala.io.Source.fromResource("time/t0").getLines.toArray
    }.toOption
    timeGenesis match {
      case Some(lines) =>
        println("\nCoordinator fetching global time...")
        val t0in:Long = lines(0).toLong
        t0 = t0in
        var notSynced = true
        while (notSynced) Try{
          val ntpClient = new NTPClient
          ntpClient.getOffset(Array(timeServer))
        } match {
          case Success(value) =>
            localClockOffset = value
            notSynced = false
          case Failure(_) =>
            println("Error: could not fetch global time, trying again...")
        }
      case None => Try{
        println("\nCoordinator loading local time data...")
        val lines = readFile(timeDataFile)
        val t0in:Long = lines.head.toLong
        val tw:Long = lines(1).toLong
        val offset =  System.currentTimeMillis()-tw
        t0 = t0in + offset
      }.orElse(Try{t0 = System.currentTimeMillis()})
    }
    globalSlot = ((globalTime - t0) / slotT).toInt
  }

  def writeTimeInfo(): Unit = {
    val file = new File(s"$storageDir/time/timeInfo")
    file.getParentFile.mkdirs
    val bw = new BufferedWriter(new FileWriter(file))
    val tw = System.currentTimeMillis()
    bw.write(s"$t0\n$tw\n")
    bw.close()
  }

  def startHolder(i:Int): ActorRefWrapper =
    ActorRefWrapper(context.actorOf(Stakeholder.props(fch.hash(Base58.encode(inputSeed)+i.toString),i,inputRef.map(_.actorRef)), "Holder_" + i.toString))

  def populate: Receive = {
    /**populates the holder list with stakeholder actor refs, the F_init functionality */
    case Populate =>
      sendAssertDone(routerRef,CoordRef(selfWrapper))
      sendAssertDone(routerRef,Register)
      sendAssertDone(localRef,CoordRef(selfWrapper))
      sendAssertDone(localRef,Populate)
      println(s"Epoch Length = $epochLength")
      println(s"Delta = $delta_s")
      println(s"K = $k_n")
      println(s"S = $slotWindow")
      if (holderIndexMin > -1 && holderIndexMax > -1) {
        println("Populating...")
        holders = List.range(holderIndexMin,holderIndexMax+1).map(startHolder)
        holdersToIssueRandomly = holders
      } else {
        println("No Holders to start...")
      }
      sendAssertDone(routerRef,HoldersFromLocal(holders,printInfo = true))
      sendAssertDone(localRef,HoldersFromLocal(holders.filterNot(_.remote),printInfo = false))
  }

  def receiveRemoteHolders: Receive = {
    case HoldersFromRemote(remoteHolders:List[ActorRefWrapper]) =>
      holders = remoteHolders
      holders.filterNot(_.remote).foreach(sendAssertDone(_,HoldersFromLocal(holders,printInfo = false)))
  }

  def restoreOrGenerateGenBlock: Receive = {
    case Register =>
      blocks.restoreBlock((0,genBlockKey)) match {
        case Some(b:Block) =>
          genesisBlock = Some(Block(hash(b.prosomoHeader,serializer),b.blockHeader,b.blockBody,b.genesisSet))
          verifyBlock(genesisBlock.get)
          println("Recovered Genesis Block")
        case None => Try{
          println("Reading genesis block from resources...")
          import scala.io.Source
          val blockTxt : Array[String] = Source.fromResource("genesis/blockData").getLines.toArray
          serializer.fromBytes(new ByteStream(Base58.decode(blockTxt.head).get,DeserializeGenesisBlock)) match {
            case b: Block =>
              genesisBlock = Some(Block(hash(b.prosomoHeader, serializer), b.blockHeader, b.blockBody, b.genesisSet))
              verifyBlock(genesisBlock.get)
              println("Recovered Genesis Block")
            case _ =>
              println("error: genesis block is corrupted")
              System.exit(0)
          }
        }.orElse(Try{forge()})
      }
      setupLocal()
  }

  def pkFromIndex(index:Int):PublicKeyW = {
    val holderIndex:String = index.toString
    val holderSeed:Array[Byte] = fch.hash(Base58.encode(inputSeed)+holderIndex)
    val rngSeed:Random = new Random
    rngSeed.setSeed(BigInt(holderSeed).toLong)
    val seed1 = fch.hash(rngSeed.nextString(32))
    val seed2 = fch.hash(rngSeed.nextString(32))
    val seed3 = fch.hash(rngSeed.nextString(32))
    Keys.seedKeysSecure(seed1,seed2,seed3,sig,vrf,kes,0).get.pkw
  }

  def pkFromIndex(holder:ActorRefWrapper):PublicKeyW = {
    val holderIndex:String = holder.actorPath.name.drop("Holder_".length)
    val holderSeed:Array[Byte] = fch.hash(Base58.encode(inputSeed)+holderIndex)
    val rngSeed:Random = new Random
    rngSeed.setSeed(BigInt(holderSeed).toLong)
    val seed1 = fch.hash(rngSeed.nextString(32))
    val seed2 = fch.hash(rngSeed.nextString(32))
    val seed3 = fch.hash(rngSeed.nextString(32))
    Keys.seedKeysSecure(seed1,seed2,seed3,sig,vrf,kes,0).get.pkw
  }

  def forge():Unit = {
    println("Forge Genesis Block")
    val holderKeys = List.range(0,numGenesisHolders).map(i =>i-> pkFromIndex(i)).toMap
    forgeGenBlock(eta0,holderKeys,pk_sig,pk_vrf,pk_kes,sk_vrf,sk_kes) match {
      case newBlock:Block => genesisBlock = Some(newBlock)
    }
    blocks.store(genBlockKey,genesisBlock.get)
    if (Parameters.writeGenBlock) {
      val file = new File("src/main/resources/genesis/blockData")
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(Base58.encode(serializer.getBytes(genesisBlock.get)))
      bw.close()
    }
  }

  def setupLocal():Unit = {
    if (holderIndexMin > -1 && holderIndexMax > -1) {
      SharedData.printingHolder = holderIndexMin
    }
    println("Sending holders list")
    sendAssertDone(holders.filterNot(_.remote),HoldersFromLocal(holders,printInfo = false))
    println("Sending holders coordinator ref")
    sendAssertDone(holders.filterNot(_.remote),CoordRef(selfWrapper))
    println("Send GenBlock")
    sendAssertDone(holders.filterNot(_.remote),GenBlock(genesisBlock.get))

  }

  def run:Receive = {
    /**sends start command to each stakeholder*/
    case Run =>
      println("Starting")
      sendAssertDone(holders.filterNot(_.remote),Initialize(globalSlot,None))
      println("Run")
      sendAssertDone(holders.filterNot(_.remote),SetClock(t0))
      if (useFencing) sendAssertDone(routerRef,SetClock(t0))
      if (useFencing) routerRef ! Run
      for (holder<-rng.shuffle(holders.filterNot(_.remote))) {
        holder ! Run
      }
      if (!useFencing) timers.startPeriodicTimer(ReadCommand, ReadCommand, commandUpdateTime)
  }

  def giveTime:Receive = {
    /**returns offset time to stakeholder that issues GetTime to coordinator*/
    case GetTime =>
      if (!actorStalled && !SharedData.limiterFlag) {
        t1 = globalTime
        sender() ! GetTime(t1)
      }
  }

  def dataFile:Receive = {
    /**coordinator creates a file writer object that is passed to stakeholders if data is being written*/
    case NewDataFile =>
      if(dataOutFlag) {
        val dataPath = Path(dataFileDir)
        Try(dataPath.createDirectory())
        val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
        val uid = uuid
        fileWriter = new BufferedWriter(new FileWriter(s"$dataFileDir/ouroboros-data-$uid-$dateString.data"))
        val fileString = (
          "Holder_number"
            + " t"
            + " alpha"
            + " blocks_forged"
            + " chain_length"
            +" \n"
          )
        fileWriter match {
          case fw: BufferedWriter => fw.write(fileString)
          case _ => println("error: file writer not initialized")
        }
      }
  }

  def nextSlot:Receive = {
    case NextSlot =>
      if (!actorPaused && !actorStalled) {
        if (roundDone) {
          t += 1
          roundDone = false
          routerRef ! NextSlot
        }
      }
  }

  /**randomly picks two holders and creates a transaction between the two*/
  def issueRandTx():Unit = {
    for (_ <- 0 to txProbability.floor.toInt) if (holders.length > 1) {
      Try{rng.shuffle(holdersToIssueRandomly).head}.toOption match {
        case Some(holder1:ActorRefWrapper) =>
          val r = rng.nextDouble
          if (r<txProbability%1.0) {
            if (SharedData.numTxsMempool<txPerBlock*3) {
              val holder2 = holders.filter(_ != holder1)(rng.nextInt(holders.length-1))
              assert(holder1 != holder2)
              val delta:BigInt = BigDecimal(maxTransfer*rng.nextDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
              holder1 ! IssueTx(holder2,delta)
              transactionCounter += 1
            }
          }
        case None =>
      }
    }
  }

  def issueTx(holder1:ActorRefWrapper, holder2:ActorRefWrapper, delta:BigInt):Unit = {
    holder1 ! IssueTx(holder2,delta)
    transactionCounter += 1
  }

  /**command string interpreter*/
  def command(sl:List[String]):Unit = {
    for (s<-sl.reverse){
      s.trim match {
        case "status_all" =>
          SharedData.txCounter = 0
          sendAssertDone(holders.filterNot(_.remote),Status)
          println("Total Transactions: "+SharedData.txCounter)
          println("Total Attempts to Issue Txs:"+transactionCounter.toString)
          SharedData.txCounter = 0
        case "fence_step" => sendAssertDone(routerRef,"fence_step")
        case "verify_all" => sendAssertDone(holders.filterNot(_.remote),Verify)
        case "stall" => sendAssertDone(holders.filterNot(_.remote),StallActor)
        case "pause" =>
          if (!actorPaused) {
            actorPaused = true
            if (!actorStalled) {
              actorStalled = true
            }
          } else {
            actorPaused = false
            if (actorStalled) {
              actorStalled = false
            }
          }
        case "inbox" => sendAssertDone(holders.filterNot(_.remote),Inbox)
        case "randtx" => if (!transactionFlag) {transactionFlag = true} else {transactionFlag = false}
        case "write" => fileWriter match {
          case fw:BufferedWriter => fw.flush()
          case _ => println("File writer not initialized")
        }
        case "tree_all" =>
          for (holder<-holders.filterNot(_.remote)) {
            printTree(holder)
          }
        case "tree" =>
          printTree(holders.filterNot(_.remote)(SharedData.printingHolder))
        case "kill" =>
          SharedData.killFlag = true
          timers.cancelAll
          fileWriter match {
            case fw:BufferedWriter => fw.close()
            case _ => println("error: file writer close on non writer object")
          }
          Thread.sleep(2*slotT*delta_s)
          context.system.terminate

        case "split" =>
          parties = List()
          val (holders1,holders2) = rng.shuffle(holders).splitAt(rng.nextInt(holders.length-2)+1)
          println("Splitting Party into groups of "+holders1.length.toString+" and "+holders2.length.toString)
          sendAssertDone(holders1,Party(holders1,clear = true))
          sendAssertDone(holders2,Party(holders2,clear = true))
          parties ::= holders1
          parties ::= holders2

        case "bridge" =>
          parties = List()
          val (holders1,holders2) = rng.shuffle(holders).splitAt(rng.nextInt(holders.length-3)+2)
          println("Bridging Party into groups of "+holders1.length.toString+" and "+holders2.length.toString)
          val commonRef = holders1.head
          sendAssertDone(holders,Party(List(),clear = true))
          sendAssertDone(List(commonRef),Party(holders,clear = false))
          sendAssertDone(holders1.tail,Party(holders1,clear = false))
          sendAssertDone(holders2,Party(commonRef::holders2,clear = false))
          parties ::= holders1
          parties ::= holders2

        case "join" =>
          parties = List()
          println("Joining Parties")
          sendAssertDone(holders.filterNot(_.remote),Party(holders,clear = true))
          parties ::= holders

        case "new_holder" =>
          println("Bootstrapping new holder...")
          val i = holders.length
          val newHolder = ActorRefWrapper(context.actorOf(Stakeholder.props(fch.hash(Base58.encode(inputSeed)+i.toString),i,inputRef.map(_.actorRef)), "Holder_" + i.toString))
          holders.find(newHolder.path == _.path) match {
            case None =>
              holders ::= newHolder
              sendAssertDone(newHolder,HoldersFromLocal(holders,printInfo = false))
              sendAssertDone(newHolder,CoordRef(selfWrapper))
              sendAssertDone(newHolder,GenBlock(genesisBlock.get))
              sendAssertDone(newHolder,Initialize(globalSlot,None))
              sendAssertDone(newHolder,SetClock(t0))
              println("Starting new holder")
              sendAssertDone(routerRef,HoldersFromLocal(holders,printInfo = true))
              sendAssertDone(localRef,HoldersFromLocal(holders.filterNot(_.remote),printInfo = false))
              sendAssertDone(holders.filterNot(_.remote),HoldersFromLocal(holders,printInfo = false))
              newHolder ! Run
            case _ => newHolder ! PoisonPill
          }

        case value:String =>
          val arg0 = "print_"
          if (value.slice(0,arg0.length) == arg0) {
            val index:Int = value.drop(arg0.length).toInt
            SharedData.printingHolder = index
          }

          val arg1 = "stall_"
          if(value.slice(0,arg1.length) == arg1) {
            val index:Int = value.drop(arg1.length).toInt
            if (index < holders.length) {
              sendAssertDone(holders(index),StallActor)
              println(s"Holder $index sent stall signal")
            }
          }

          val arg2 = "split_stake_"
          if(value.slice(0,arg2.length) == arg2) {
            val ratio:Double =  value.drop(arg2.length).toDouble
            assert(ratio>0.0 && ratio<1.0)
            val stakingState:State = getStakingState(holders.head)
            val netStake:BigInt = {
              var out:BigInt = 0
              for (holder<-holders){
                out += stakingState(pkFromIndex(holder))._1
              }
              out
            }

            var holders1:List[ActorRefWrapper] = List()
            var net1:BigInt = 0
            var holders2:List[ActorRefWrapper] = List()
            var net2:BigInt = 0
            for (holder <- rng.shuffle(holders)) {
              val holderStake = stakingState(pkFromIndex(holder))._1
              if (net1< BigDecimal(ratio*(net1.toDouble+net2.toDouble)).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt) {
                net1 += holderStake
                holders1 ::= holder
              } else {
                net2 += holderStake
                holders2 ::= holder
              }
            }
            val alpha1 = net1.toDouble/netStake.toDouble
            val alpha2 = net2.toDouble/netStake.toDouble
            val numh1 = holders1.length
            val numh2 = holders2.length

            parties = List()

            println(s"Splitting Stake to $alpha1 and $alpha2 with $numh1 and $numh2 holders")
            sendAssertDone(holders1,Party(holders1,clear = true))
            sendAssertDone(holders2,Party(holders2,clear = true))
            parties ::= holders1
            parties ::= holders2
          }

          val arg3 = "bridge_stake_"
          if (value.slice(0,arg3.length) == arg3) {
            val ratio:Double =  value.drop(arg3.length).toDouble
            assert(ratio>0.0 && ratio<1.0)
            val stakingState:State = getStakingState(holders.head)
            val netStake:BigInt = {
              var out:BigInt = 0
              for (holder<-holders){
                out += stakingState(pkFromIndex(holder))._1
              }
              out
            }
            var holders1:List[ActorRefWrapper] = List()
            var net1:BigInt = 0
            var holders2:List[ActorRefWrapper] = List()
            var net2:BigInt = 0
            for (holder <- rng.shuffle(holders)) {
              val holderStake = stakingState(pkFromIndex(holder))._1
              if (net1<BigDecimal(ratio*(net1.toDouble+net2.toDouble)).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt) {
                net1 += holderStake
                holders1 ::= holder
              } else {
                net2 += holderStake
                holders2 ::= holder
              }
            }
            val alpha1 = net1.toDouble/netStake.toDouble
            val alpha2 = net2.toDouble/netStake.toDouble
            val numh1 = holders1.length
            val numh2 = holders2.length

            parties = List()

            println(s"Bridging Stake to $alpha1 and $alpha2 with $numh1 and $numh2 holders")
            val commonRef = holders1.head
            sendAssertDone(holders,Party(List(),clear = true))
            sendAssertDone(List(commonRef),Party(holders,clear = false))
            sendAssertDone(holders1.tail,Party(holders1,clear = false))
            sendAssertDone(holders2,Party(commonRef::holders2,clear = false))
            parties ::= holders1
            parties ::= holders2
          }

          val arg4 = "issue_"
          if (value.slice(0,arg4.length) == arg4) {
            val data:String = value.drop(arg4.length)
            val hIndex1s:String = data.split("_")(0)
            val hIndex2s:String = data.split("_")(1)
            val deltas:String = data.split("_")(2)
            val holder1:ActorRefWrapper = holders(hIndex1s.toInt)
            val holder2:ActorRefWrapper = holders(hIndex2s.toInt)
            val delta:BigInt = BigInt(deltas)
            issueTx(holder1,holder2,delta)
          }

          val arg5 = "balance_"
          if (value.slice(0,arg5.length) == arg5) {
            val data = value.drop(arg5.length)
            holders(data.toInt) ! GetBalance
          }

          val arg6 = "status_"
          if (value.slice(0,arg6.length) == arg6) {
            val data = value.drop(arg6.length)
            sendAssertDone(holders(data.toInt),Status)
            println("Total Txs:"+transactionCounter.toString)
          }

          val arg7 = "verify_"
          if (value.slice(0,arg7.length) == arg7) {
            val data = value.drop(arg7.length)
            sendAssertDone(holders(data.toInt),Verify)
          }

          val arg8 = "adversary_"
          if (value.slice(0,arg8.length) == arg8) {
            val data = value.drop(arg8.length)
            sendAssertDone(holders(data.toInt),Adversary(""))
          }

          val arg9 = "covert_"
          if (value.slice(0,arg9.length) == arg9) {
            val data = value.drop(arg9.length)
            sendAssertDone(holders(data.toInt),Adversary("covert"))
          }

          val arg10 = "nas_"
          if(value.slice(0,arg10.length) == arg10) {
            val data = value.drop(arg10.length)
            sendAssertDone(holders(data.toInt),Adversary("nas"))
          }

          val arg11 = "new_holder_"
          if (value.slice(0,arg11.length) == arg11) {
            val data = value.drop(arg11.length)
            println("Bootstrapping new holder...")
            val i = data.toInt
            val newHolder = ActorRefWrapper(context.actorOf(Stakeholder.props(fch.hash(Base58.encode(inputSeed)+i.toString),i,inputRef.map(_.actorRef)), "Holder_" + i.toString))
            holders.find(newHolder.path == _.path) match {
              case None =>
                holders ::= newHolder
                sendAssertDone(newHolder,HoldersFromLocal(holders,printInfo = false))
                sendAssertDone(newHolder,CoordRef(selfWrapper))
                sendAssertDone(newHolder,GenBlock(genesisBlock.get))
                sendAssertDone(newHolder,Initialize(globalSlot,None))
                sendAssertDone(newHolder,SetClock(t0))
                println("Starting new holder")
                sendAssertDone(routerRef,HoldersFromLocal(holders,printInfo = true))
                sendAssertDone(localRef,HoldersFromLocal(holders.filterNot(_.remote),printInfo = false))
                sendAssertDone(holders.filterNot(_.remote),HoldersFromLocal(holders,printInfo = false))
                newHolder ! Run
              case _ => newHolder ! PoisonPill
            }

          }
        case _ =>
      }
    }
  }

  def readCommand():Unit = {
    if (!useFencing) {
      if (!actorStalled) {
        t1 = globalTime
        t = ((t1 - t0) / slotT).toInt
      }
    }
    if (t>globalSlot) {
      writeTimeInfo()
      globalSlot = t
      SharedData.globalSlot = globalSlot
      SharedData.diskAccess = false
    }

    if (devMode) {
      if (new File("command/cmd").exists) {
        println("-----------------------------------------------------------")
        val f = new File("command/cmd")
        val cmd: String = ("cat" #< f).!!
        f.delete
        val cmdList = cmd.split("\n")
        for (line<-cmdList) {
          val com = line.trim.split(" ")
          com(0) match {
            case s:String =>
              if (com.length == 2) {
                Try{com(1).toInt}.toOption match {
                  case Some(i:Int) =>
                    if (cmdQueue.keySet.contains(i)) {
                      val nl = s::cmdQueue(i)
                      cmdQueue -= i
                      cmdQueue += (i->nl)
                    } else {
                      cmdQueue += (i->List(s))
                    }
                  case None =>
                }
              } else {
                if (cmdQueue.keySet.contains(t)){
                  val nl = s::cmdQueue(t)
                  cmdQueue -= t
                  cmdQueue += (t->nl)
                } else {
                  cmdQueue += (t->List(s))
                }
              }
            case _ =>
          }
        }
      }
    }

    if (cmdQueue.keySet.contains(t)) {
      command(cmdQueue(t))
      cmdQueue -= t
    }


    if (!actorStalled && transactionFlag && !useFencing && t>1 && !SharedData.errorFlag && !SharedData.limiterFlag) {
      issueRandTx()
    }

    if (SharedData.killFlag) {
      timers.cancelAll
      context.system.terminate
      System.exit(0)
    }
  }

  def readCommand(cmd:String):Unit = {
    if (!useFencing) {
      if (!actorStalled) {
        t1 = globalTime
        t = ((t1 - t0) / slotT).toInt
      }
    }
    if (t>globalSlot) {
      writeTimeInfo()
      globalSlot = t
      SharedData.diskAccess = false
    }
    println("-----------------------------------------------------------")
    val cmdList = cmd.split("\n")
    for (line<-cmdList) {
      val com = line.trim.split(" ")
      com(0) match {
        case s:String =>
          if (com.length == 2){
            Try{com(1).toInt}.toOption match {
              case Some(i:Int) =>
                if (cmdQueue.keySet.contains(i)) {
                  val nl = s::cmdQueue(i)
                  cmdQueue -= i
                  cmdQueue += (i->nl)
                } else {
                  cmdQueue += (i->List(s))
                }
              case None =>
            }
          } else {
            if (cmdQueue.keySet.contains(t)){
              val nl = s::cmdQueue(t)
              cmdQueue -= t
              cmdQueue += (t->nl)
            } else {
              cmdQueue += (t->List(s))
            }
          }
        case _ =>
      }
    }
    if (cmdQueue.keySet.contains(t)) {
      command(cmdQueue(t))
      cmdQueue -= t
    }
  }


  def printTree(holder:ActorRefWrapper):Unit = {
    var tn = 0
    if (useFencing) {
      tn = t
    } else {
      tn = ((t1 - t0) / slotT).toInt
    }
    blockTree(holder)
    val positionData:(Map[ActorRefWrapper,(Double,Double)],Map[(ActorRefWrapper,ActorRefWrapper),Long]) = getPositionData(routerRef)
    val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
    val uid = uuid
    val holderIndex = holders.indexOf(holder)
    graphWriter = new BufferedWriter(new FileWriter(s"$dataFileDir/ouroboros-holder-$holderIndex-$uid-$dateString.tree"))
    graphWriter match {
      case fw:BufferedWriter =>
        fw.flush()
      case _ =>
    }
    graphWriter match {
      case fw:BufferedWriter =>
        fw.close()
      case _ =>
    }
  }

  def newHolderFromUI:Receive = {
    case NewHolderFromUI(kf,ddir,pwd,name,kdir) =>
      val i = holders.filterNot(_.remote).size
      println(s"Bootstrapping Holder $i...")
      SharedData.printingHolder = i
      val newHolder = ActorRefWrapper(context.actorOf(Stakeholder.props(fch.hash(uuid),i,inputRef.map(_.actorRef),kf,ddir,pwd,kdir), name))
      holders.find(newHolder.path == _.path) match {
        case None =>
          holders ::= newHolder
          sendAssertDone(newHolder,HoldersFromLocal(holders,printInfo = false))
          sendAssertDone(newHolder,CoordRef(selfWrapper))
          sendAssertDone(newHolder,GenBlock(genesisBlock.get))
          sendAssertDone(newHolder,Initialize(globalSlot,None))
          sendAssertDone(newHolder,SetClock(t0))
          sendAssertDone(routerRef,HoldersFromLocal(holders,printInfo = true))
          sendAssertDone(localRef,HoldersFromLocal(holders.filterNot(_.remote),printInfo = false))
          sendAssertDone(holders.filterNot(_.remote),HoldersFromLocal(holders,printInfo = false))
          newHolder ! Run
          println("Forging started.")
        case _ => newHolder ! PoisonPill
      }
  }

  def auxCoordFunc:Receive = {
    /**tells actors to print their inbox */
    case Inbox => sendAssertDone(holders.filterNot(_.remote),Inbox)
    /**passes fileWriter to actor who requests it with WriteFile*/
    case WriteFile => sender() ! WriteFile(fileWriter)
    /**closes the writer object to make sure data is written from buffer*/
    case CloseDataFile => if(dataOutFlag) {fileWriter match {
      case fw:BufferedWriter => fw.close()
      case _ => println("error: file writer close on non writer object")}}
    case EndStep => readCommand(); roundDone = true

    /**command interpretation from config and cmd script*/
    case ReadCommand => readCommand()
    case GuiCommand(s) => readCommand(s)
    case any:Any => {
      print("Error: Coordinator received unknown message ")
      println(any.getClass.toString+" "+any.toString)
    }
  }

  override def receive:Receive = giveTime orElse
    newHolderFromUI orElse
    run orElse
    populate orElse
    receiveRemoteHolders orElse
    restoreOrGenerateGenBlock orElse
    nextSlot orElse
    dataFile orElse
    auxCoordFunc
}

object Coordinator {
  def props(inputSeed:Array[Byte],ref:Seq[akka.actor.ActorRef]): Props =
    Props(new Coordinator(inputSeed,ref.map(ActorRefWrapper(_)(ActorRefWrapper.routerRef(ref.head)))))
      .withDispatcher(Parameters.coordinatorEC)
}
