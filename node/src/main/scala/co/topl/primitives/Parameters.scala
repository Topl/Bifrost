package co.topl.primitives

import java.io.{BufferedReader, File, InputStreamReader}
import java.net.URL

import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import co.topl.remote._
import co.topl.primitives.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.math.BigInt
import scala.util.{Failure, Success, Try}

object Parameters {
  val fch = new Fch
  val ecx = new Ecx
  val dataBaseCID: ByteArrayWrapper = ByteArrayWrapper(fch.hash("LOCAL_CHAIN"))
  val tetraNodeUID:String = Base58.encode(fch.hash(java.util.UUID.randomUUID.toString))
  //tag for identifying ledger entries
  val genesisBytes: ByteArrayWrapper = ByteArrayWrapper(fch.hash("GENESIS".getBytes))
  println("Checking external IP at http://checkip.amazonaws.com")
  val declaredAddressFromRemote: Option[String] = Try{
    implicit val timeout:Timeout = Timeout(1.seconds)
    val future = Future({
      val whatismyip = new URL("http://checkip.amazonaws.com")
      val in:BufferedReader = new BufferedReader(new InputStreamReader(
        whatismyip.openStream()))
      in.readLine()
    })
    val result = Await.result(future,timeout.duration)
    result
  }.toOption

  def getConfig:Config = {
    val baseConfig = ConfigFactory.load
    var localConfig = baseConfig

    Try{localConfig.getString("scorex.network.agentName")}.toOption match {
      case Some(name) if name != "bootstrap" =>
        val str = "input{scorex{network{agentName=\""+s"${name}_${tetraNodeUID.take(8)}"+"\"}}}"
        Try{
          localConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(localConfig)
        }.toOption match {
          case None => println("Error: input not parsed")
          case _ =>
        }
      case Some(name) if name == "bootstrap" =>
      case None =>
        val str = "input{scorex{network{agentName=\""+s"tetra_${tetraNodeUID.take(8)}"+"\"}}}"
        Try{
          localConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(localConfig)
        }.toOption match {
          case None => println("Error: input not parsed")
          case _ =>
        }
    }
    localConfig
  }

  lazy val config:Config = getConfig

  val inputCommands:Map[Int,List[String]] = if (config.hasPath("command")) {
    var out:Map[Int,List[String]] = Map()
    val cmdList = config.getStringList("command.cmd").asScala.toList
    for (line<-cmdList) {
      val com = line.trim.split(" ")
      com(0) match {
        case s:String =>
          if (com.length == 2){
            Try{com(1).toInt}.toOption match {
              case Some(i:Int) =>
                if (out.keySet.contains(i)) {
                  val nl = s::out(i)
                  out -= i
                  out += (i->nl)
                } else {
                  out += (i->List(s))
                }
              case None =>
            }
          }
        case _ =>
      }
    }
    out
  } else {
    Map()
  }
  val devMode:Boolean = config.getBoolean("params.devMode")
  var useGui:Boolean = config.getBoolean("params.useGui")
  val timeScale:Double = config.getDouble("params.timeScale")
  //use network delay parameterization if true
  val useDelayParam:Boolean = config.getBoolean("params.useDelayParam")
  //number of stakeholders
  val numGenesisHolders:Int = config.getInt("params.numHolders")
  //minumum index of local holders, set to -1 for all local
  val holderIndexMin:Int = config.getInt("params.holderIndexMin")
  //maximum index of local holders, set to -1 for all local
  val holderIndexMax:Int = config.getInt("params.holderIndexMax")
  //duration of slot in milliseconds
  val slotT:Long = (config.getInt("params.slotT")*timeScale).toLong
  //interval between return blocks in tine provider
  val requestTineInterval:Int = config.getInt("params.requestTineInterval")
  //delay in milliseconds per kilometer in router model
  val delay_ms_km:Double = config.getDouble("params.delay_ms_km")*timeScale
  //delay in milliseconds per bit in router model
  val delay_ms_byte:Double = config.getDouble("params.delay_ms_byte")*timeScale
  //network random noise max
  val delay_ms_noise:Double = config.getDouble("params.delay_ms_noise")*timeScale
  //communication method
  val useRouting:Boolean = config.getBoolean("params.useRouting")
  //delay in slots, calculated as maximum possible delay in random global network model
  val delta_s:Int = (40075.0*delay_ms_km/slotT+delay_ms_noise/slotT+1.0).ceil.toInt
  //epoch parameter
  val epsilon_s:Double = config.getDouble("params.epsilon_s")
  //alert stake ratio
  val alpha_s:Double = config.getDouble("params.alpha_s")
  //participating stake ratio
  val beta_s:Double = config.getDouble("params.beta_s")
  //active slot coefficient
  val f_s:Double = if (useDelayParam) {
    val out = 1.0-math.exp(1.0/(delta_s+1.0))*(1+epsilon_s)/(2.0*alpha_s)
    assert(out>0)
    out
  } else {
    config.getDouble("params.f_s")
  }
  //order of accuracy for convergent series
  val o_n:Int = config.getInt("params.o_n")
  def log_one_minus(f:Ratio):Ratio = {
    //calculate log(1-f)
    var out = Ratio(0)
    for (n<- 1 to o_n) {
      out = out - ( f.pow(n)  / n )
    }
    out
  }
  val m_f:Ratio = log_one_minus(Ratio(f_s,4))
  val f_dynamic:Boolean = config.getBoolean("params.f_dynamic")
  val testStrategy:String = config.getString("params.testStrategy")
  val f_A:Ratio = Ratio(config.getDouble("params.f_A"),4)
  val f_B:Ratio = Ratio(config.getDouble("params.f_B"),4)
  val gamma:Int = config.getInt("params.gamma")
  val kappa:Int = config.getInt("params.kappa")
  val slot_gap:Int = config.getInt("params.slot_gap")
  val useMaxValidTK:Boolean = config.getBoolean("params.useMaxValidTK")
  val k_bar:Int = config.getInt("params.k_bar")
  val forging_window:Int = config.getInt("params.forging_window")
  //Local Dynamic Difficulty curve
  def ldd_gap_sawtooth(i:Int):Ratio = {
    if (i <= slot_gap) {
      Ratio(0)
    } else {
      Ratio(i-slot_gap,gamma-slot_gap)*f_A
    }
  }
  val m_f_B:Ratio = log_one_minus(f_B)
  val m_f_range:Array[Ratio] = (0 to gamma).toArray
    .map(i => ldd_gap_sawtooth(i))
    .map(f => log_one_minus(f))
  println("Difficulty curve:")
  (0 to gamma).toArray
    .map(i => println(i, ldd_gap_sawtooth(i).toBigDecimal.toString()) )

  // checkpoint depth in slots, k parameter in maxValid-bg, k > 192*delta/epsilon*beta
  val k_n:Int = if(useDelayParam) {
    (192.0*delta_s/(epsilon_s*beta_s)).floor.toInt + 1
  } else {
    config.getInt("params.k_n")
  }
  // epoch length R >= 3k/2f
  val epochLength:Int = if (useDelayParam) {
    3*(k_n*(0.5/f_s)).toInt
  } else {
    config.getInt("params.epochLength")
  }
  val one_third_epoch:Int = epochLength/3
  val one_ninth_epoch:Int = epochLength/9
  // slot window for chain selection, s = k/4f
  val slotWindow:Int = if (useDelayParam) {
    (k_n*0.25/f_s).toInt
  } else {
    config.getInt("params.slotWindow")
  }
  //status and verify check chain hash data up to this depth to gauge consensus amongst actors
  val confirmationDepth:Int = config.getInt("params.confirmationDepth")
  //max initial stake
  val initStakeMax:Double = config.getDouble("params.initStakeMax")
  //max random transaction delta
  val maxTransfer:Double = config.getDouble("params.maxTransfer")
  //reward for forging blocks
  val forgerReward:BigInt = BigInt(config.getLong("params.forgerReward"))
  //percent of transaction amount taken as fee by the forger
  val transactionFee:Double = config.getDouble("params.transactionFee")
  val fee_r:Ratio = Ratio(transactionFee,8)
  //number of holders on gossip list for sending new blocks and transactions
  val numGossipers:Int = config.getInt("params.numGossipers")
  //number of holders to gossip to upon forging
  val numGossipersForge:Int = config.getInt("params.numGossipersForge")
  //max number of tries for a tine to ask for parent blocks
  val tineMaxTries:Int = config.getInt("params.tineMaxTries")
  //max depth in multiples of confirmation depth that can be returned from an actor
  val tineMaxDepth:Int = config.getInt("params.tineMaxDepth")
  //max depth to trigger bootstrapping
  val tineBootstrappingDepth:Int = config.getInt("params.tineBootstrappingDepth")
  //data write interval in slots
  val dataOutInterval:Int = epochLength
  //time out for dropped messages from coordinator
  val waitTime:FiniteDuration = config.getInt("params.waitTime").seconds
  //duration between update tics that stakeholder actors send to themselves
  val updateTime:FiniteDuration = config.getInt("params.updateTime").millis
  //duration between command read tics and transaction generation for the coordinator
  val commandUpdateTime:FiniteDuration = config.getInt("params.commandUpdateTime").millis
  //number of txs per block
  val txPerBlock:Int = config.getInt("params.txPerBlock")
  //Issue random transactions if true
  var transactionFlag:Boolean = config.getBoolean("params.transactionFlag")
  // p = txProbability => (1-p)^numHolders
  var txProbability:Double = config.getDouble("params.txProbability")
  //uses randomness for public key seed and initial stake, set to false for deterministic run
  val randomFlag:Boolean = config.getBoolean("params.randomFlag")
  //when true, if system cpu load is too high the coordinator will stall to allow stakeholders to catch up
  val performanceFlag:Boolean = config.getBoolean("params.performanceFlag")
  //threshold of cpu usage above which coordinator will stall if performanceFlag = true
  val systemLoadThreshold:Double = config.getDouble("params.systemLoadThreshold")
  //number of values to average for load threshold
  val numAverageLoad:Int = config.getInt("params.numAverageLoad")
  //print Stakeholder 0 status per slot if true
  val printFlag:Boolean = config.getBoolean("params.printFlag")
  //print Stakeholder 0 execution time per slot if true
  val timingFlag:Boolean = config.getBoolean("params.timingFlag")
  //Record data if true, plot data points with ./cmd.sh and enter command: plot
  val dataOutFlag:Boolean = config.getBoolean("params.dataOutFlag")
  //toggle for action based round execution
  val useFencing:Boolean = config.getBoolean("params.useFencing")
  //seed for pseudo random runs
  val inputSeedString:String = {
    if (randomFlag) {
      Base58.encode(fch.hash(java.util.UUID.randomUUID.toString))
    } else {
      config.getString("params.inputSeed")
    }
  }
  //interval in slots between localChain save to disk
  val chainStoreInterval:Int = config.getInt("params.chainStoreInterval")
  //number of message processors
  val numMessageProcessors:Int = config.getInt("params.numMessageProcessors")
  //node secret for HMAC used in each router actor
  val sk_ecx:Array[Byte] = ecx.generateSK
  //public key for HMAC used in each router actor
  val pk_ecx:Array[Byte] = ecx.scalarMultBasePoint(sk_ecx)

  val stakeholderEC:String = config.getString("params.stakeholderEC")
  val routerEC:String = config.getString("params.routerEC")
  val coordinatorEC:String = config.getString("params.coordinatorEC")
  val tineProviderEC:String = config.getString("params.tineProviderEC")
  val networkControllerEC:String = config.getString("params.networkControllerEC")
  val useRouterSystem:Boolean = config.getBoolean("params.useRouterSystem")
  val maxBlockNumber:Int = config.getInt("params.maxBlockNumber")
  val writeGenBlock:Boolean = config.getBoolean("params.writeGenBlock")
  val resourceScale:Double = config.getDouble("params.resourceScale")
  val simLabel:String = config.getString("params.simLabel")
  val kesStoreInterval:Int = config.getInt("params.kesStoreInterval")
  val useStableIntervalTerm:Boolean = config.getBoolean("params.useStableIntervalTerm")

  //path for data output files
  val dataFileDir:String = config.getString("params.dataFileDir")+"/seed_"+inputSeedString
  val storageFlag:Boolean = config.getBoolean("params.storageFlag")
  val cacheSize:Int = config.getInt("params.cacheSize")
  val refreshInterval:Int = config.getInt("params.refreshInterval")
  val stakeDistribution:String = config.getString("params.stakeDistribution")
  val stakeScale:Double = config.getDouble("params.stakeScale")
  val initStakeMin:Double = config.getDouble("params.initStakeMin")
  val timeServer:String = config.getString("params.timeServer")
  val tetraMessageSpecs = Seq(
    DiffuseDataSpec,
    HelloSpec,
    RequestBlockSpec,
    RequestTineSpec,
    ReturnBlocksSpec,
    SendBlockSpec,
    SendTxSpec,
    HoldersFromRemoteSpec
  )
}
