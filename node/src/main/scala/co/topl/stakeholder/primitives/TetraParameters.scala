package co.topl.stakeholder.primitives

import java.io.{BufferedReader, InputStreamReader}
import java.net.URL

import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.duration._
import co.topl.stakeholder.remote._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.Try

object TetraParameters {
  val fch = new Fch
  val ecx = new Ecx
  val dataBaseCID: ByteArrayWrapper = ByteArrayWrapper(fch.hash("LOCAL_CHAIN"))
  val tetraNodeUID: String = Base58.encode(fch.hash(java.util.UUID.randomUUID.toString))
  //tag for identifying ledger entries
  val genesisBytes: ByteArrayWrapper = ByteArrayWrapper(fch.hash("GENESIS".getBytes))
  println("Checking external IP at http://checkip.amazonaws.com")

  val declaredAddressFromRemote: Option[String] = Try {
    implicit val timeout: Timeout = Timeout(1.seconds)
    val future = Future({
      val whatismyip = new URL("http://checkip.amazonaws.com")
      val in: BufferedReader = new BufferedReader(new InputStreamReader(whatismyip.openStream()))
      in.readLine()
    })
    val result = Await.result(future, timeout.duration)
    result
  }.toOption

  def getConfig: Config = {
    val baseConfig = ConfigFactory.load
    var localConfig = baseConfig

    Try(localConfig.getString("scorex.network.agentName")).toOption match {
      case Some(name) if name != "bootstrap" =>
        val str = "input{scorex{network{agentName=\"" + s"${name}_${tetraNodeUID.take(8)}" + "\"}}}"
        Try {
          localConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(localConfig)
        }.toOption match {
          case None => println("Error: input not parsed")
          case _    =>
        }
      case Some(name) if name == "bootstrap" =>
      case None =>
        val str = "input{scorex{network{agentName=\"" + s"tetra_${tetraNodeUID.take(8)}" + "\"}}}"
        Try {
          localConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(localConfig)
        }.toOption match {
          case None => println("Error: input not parsed")
          case _    =>
        }
    }
    localConfig
  }

  lazy val config: Config = getConfig

  //duration of slot in milliseconds
  val slotT: Long = (config.getInt("params.slotT")).toLong
  //interval between return blocks in tine provider
  val requestTineInterval: Int = config.getInt("params.requestTineInterval")
  //active slot coefficient
  val f_s: Double = config.getDouble("params.f_s")
  //order of accuracy for convergent series
  val o_n: Int = config.getInt("params.o_n")

  def log_one_minus(f: Ratio): Ratio = {
    //calculate log(1-f)
    var out = Ratio(0)
    for (n <- 1 to o_n)
      out = out - (f.pow(n) / n)
    out
  }
  val m_f: Ratio = log_one_minus(Ratio(f_s, 4))
  val f_dynamic: Boolean = config.getBoolean("params.f_dynamic")
  val testStrategy: String = config.getString("params.testStrategy")
  val f_A: Ratio = Ratio(config.getDouble("params.f_A"), 4)
  val f_B: Ratio = Ratio(config.getDouble("params.f_B"), 4)
  val gamma: Int = config.getInt("params.gamma")
  val kappa: Int = config.getInt("params.kappa")
  val slot_gap: Int = config.getInt("params.slot_gap")
  val useMaxValidTK: Boolean = config.getBoolean("params.useMaxValidTK")
  val k_bar: Int = config.getInt("params.k_bar")
  val forging_window: Int = config.getInt("params.forging_window")

  //Local Dynamic Difficulty curve
  def snowplow_curve(i: Int): Ratio =
    if (i <= slot_gap) {
      Ratio(0)
    } else {
      Ratio(i - slot_gap, gamma - slot_gap) * f_A
    }
  val m_f_B: Ratio = log_one_minus(f_B)

  val m_f_range: Array[Ratio] = (0 to gamma).toArray
    .map(i => snowplow_curve(i))
    .map(f => log_one_minus(f))

  // checkpoint depth in blocks, k parameter in maxValid-bg, k > 192*delta/epsilon*beta
  val k_n: Int = config.getInt("params.k_n")
  // epoch length R >= 3k/2f_eff
  val epochLength: Int = config.getInt("params.epochLength")
  val one_third_epoch: Int = epochLength / 3
  val one_ninth_epoch: Int = epochLength / 9
  // slot window for chain selection, s = k/4f
  val slotWindow: Int = config.getInt("params.slotWindow")
  //number of holders on gossip list for sending new blocks and transactions
  val numGossipers: Int = config.getInt("params.numGossipers")
  //number of holders to gossip to upon forging
  val numGossipersForge: Int = config.getInt("params.numGossipersForge")
  //max number of tries for a tine to ask for parent blocks
  val tineMaxTries: Int = config.getInt("params.tineMaxTries")
  //max depth in multiples of confirmation depth that can be returned from an actor
  val tineMaxDepth: Int = config.getInt("params.tineMaxDepth")
  //max depth to trigger bootstrapping
  val tineBootstrappingDepth: Int = config.getInt("params.tineBootstrappingDepth")
  //time out for dropped messages from coordinator
  val waitTime: FiniteDuration = config.getInt("params.waitTime").seconds
  //duration between update tics that stakeholder actors send to themselves
  val updateTime: FiniteDuration = config.getInt("params.updateTime").millis
  //interval in slots between localChain save to disk
  val chainStoreInterval: Int = config.getInt("params.chainStoreInterval")
  //number of message processors
  val numMessageProcessors: Int = config.getInt("params.numMessageProcessors")
  //node secret for HMAC used in each router actor
  val sk_ecx: Array[Byte] = ecx.generateSK
  //public key for HMAC used in each router actor
  val pk_ecx: Array[Byte] = ecx.scalarMultBasePoint(sk_ecx)

  val stakeholderEC: String = config.getString("params.stakeholderEC")
  val routerEC: String = config.getString("params.routerEC")
  val coordinatorEC: String = config.getString("params.coordinatorEC")
  val tineProviderEC: String = config.getString("params.tineProviderEC")
  val networkControllerEC: String = config.getString("params.networkControllerEC")
  val useRouterSystem: Boolean = config.getBoolean("params.useRouterSystem")
  val kesStoreInterval: Int = config.getInt("params.kesStoreInterval")

  //path for data output files
  val dataFileDir: String = config.getString("params.dataFileDir")
  val cacheSize: Int = config.getInt("params.cacheSize")
  val refreshInterval: Int = config.getInt("params.refreshInterval")
  val timeServer: String = config.getString("params.timeServer")

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
