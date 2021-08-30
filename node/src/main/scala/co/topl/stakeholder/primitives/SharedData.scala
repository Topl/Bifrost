package co.topl.stakeholder.primitives

import java.io.ByteArrayOutputStream
import java.io.PrintStream

/**
  * AMS 2020:
  * Singlton object for quick sharing of references and data between actors with no message passing
  * Used for GUI information, the printing stakeholder writes to members that the GUI reads
  * Includes timing tools and aux information for debugging
 */

object SharedData extends Types {
  var counter = 0
  var errorFlag = false
  var killFlag = false
  var txCounter = 0
  var printingHolder = 0
  var t_0:Long = 0
  var t_1:Long = 0
  var timing:Boolean = false
  var diskAccess:Boolean = false
  var limiterFlag:Boolean = false
  val fch:Fch = new Fch
  var globalSlot = 0

  val outText = new ByteArrayOutputStream
  val printStream = new PrintStream(outText)
  val oldOut: PrintStream = System.out

  def time0(): Unit = {
    timing = true
    t_0 = System.nanoTime()
  }

  def time1(): Unit = if (timing) {
    t_1 = System.nanoTime()
    timing = false
    val outTime = (t_1 - t_0)*1.0e-9
    val tString = "%6.6f".format(outTime)
    println("Elapsed time: " + tString + " s")
  }

  def count:Int = {
    val out = counter
    counter += 1
    println(counter)
    out
  }

  def throwDiskWarning(text:String): Unit = if (!diskAccess) {
    println(Console.YELLOW + s"Disk access: $text" + Console.RESET)
    diskAccess = true
  }

  def throwLimiterWarning(text:String): Unit = if (!diskAccess) {
    text match {
      case "Start" if !limiterFlag =>
        println(Console.YELLOW + s"Limiter: $text" + Console.RESET)
        limiterFlag = true
      case "Stop" if limiterFlag =>
        println(Console.YELLOW + s"Limiter: $text" + Console.RESET)
        limiterFlag = false
      case _ =>
    }
  }

  def throwError(id:Int): Unit = {println(s"Holder $id ---------Error----------");errorFlag=true;killFlag=true}
  def throwError(): Unit = {println("---------Error----------")}
  def error:Boolean = errorFlag


  var activePeers:Int = 0
  var activeStake:Double = 0.0
  var blockTime:Double = 0.0
  var txsPerSecond:Double = 0.0
  var numTxsMempool:Int = 0
  var activeSlots:Double = 0.0
  var averageNetworkDelay:Double = 0.0
  var minNetworkDelay:Double = 0.0
  var maxNetworkDelay:Double = 0.0
  var averageTineLength:Double = 0.0
  var maxTineLength:Int = 0

  var guiPeerInfo:Map[String,List[ActorRefWrapper]] = Map()
  var confirmedBalance:Map[String,BigInt] = Map()
  var stakingBalance:Map[String,BigInt] = Map()
  var confirmedAlpha:Map[String,Double] = Map()
  var stakingAlpha:Map[String,Double] = Map()
  var walletInfo:(Int,Int,BigInt,BigInt) = (0,0,0,0)
  var issueTxInfo:Option[(PublicKeyW,Map[Sid,(ActorRefWrapper,PublicKeys)])] = None
  var selfWrapper:Option[ActorRefWrapper] = None
}

