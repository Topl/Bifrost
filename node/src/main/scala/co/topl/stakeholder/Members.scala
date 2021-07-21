package co.topl.stakeholder

import akka.actor.{Actor, ActorPath, Timers}
import com.google.common.cache.LoadingCache
import io.iohk.iodb.ByteArrayWrapper
import co.topl.primitives.{ActorRefWrapper, Fch, Kes, KeyFile, Keys, Parameters, Ratio, Sig, SimpleTypes, Vrf}
import co.topl.components.{Block, Serializer, Tine, Transaction, Wallet}
import co.topl.history.{BlockStorage, ChainStorage, StateStorage, WalletStorage}
import co.topl.settings.AppSettings

import scala.concurrent.duration._
import scala.math.BigInt
import scala.util.Random

/**
  * AMS 2020:
  * The root trait for actors that will participate in consensus
  * All members required to execute Ouroboros routines and maintain state, mempool, tinepool, and wallet
  */

trait Members extends SimpleTypes with Actor with Timers {

  implicit val routerRef:ActorRefWrapper
  implicit val blocks:BlockStorage

  val devMode:Boolean = Parameters.devMode
  var useGui:Boolean = Parameters.useGui
  val dataBaseCID: ByteArrayWrapper = Parameters.dataBaseCID
  val genesisBytes: ByteArrayWrapper = Parameters.genesisBytes
  val inputSeedString:String = Parameters.inputSeedString
  val numGenesisHolders:Int = Parameters.numGenesisHolders
  val holderIndexMin:Int = Parameters.holderIndexMin
  val holderIndexMax:Int = Parameters.holderIndexMax
  val slotT:Long = Parameters.slotT
  val useRouting:Boolean = Parameters.useRouting
  val delta_s:Int = Parameters.delta_s
  val o_n:Int = Parameters.o_n
  val m_f:Ratio = Parameters.m_f
  val f_dynamic:Boolean = Parameters.f_dynamic
  val testStrategy:String = Parameters.testStrategy
  val m_f_range:Array[Ratio] = Parameters.m_f_range
  val m_f_B:Ratio = Parameters.m_f_B
  val gamma:Slot = Parameters.gamma
  val slot_gap:Slot = Parameters.slot_gap
  val kappa:Int = Parameters.kappa
  val k_bar:Int = Parameters.k_bar
  val useMaxValidTK:Boolean = Parameters.useMaxValidTK
  val k_n:Int = Parameters.k_n
  val epochLength:Int = Parameters.epochLength
  val one_third_epoch:Int = Parameters.one_third_epoch
  val slotWindow:Int = Parameters.slotWindow
  val confirmationDepth:Int = Parameters.confirmationDepth
  val initStakeMax:Double = Parameters.initStakeMax
  val maxTransfer:Double = Parameters.maxTransfer
  val forgerReward:BigInt = Parameters.forgerReward
  val fee_r:Ratio = Parameters.fee_r
  val numGossipers:Int = Parameters.numGossipers
  val numGossipersForge:Int = Parameters.numGossipersForge
  val tineMaxTries:Int = Parameters.tineMaxTries
  val tineMaxDepth:Int = Parameters.tineMaxDepth
  val tineBootstrappingDepth:Int = Parameters.tineBootstrappingDepth
  val dataOutInterval:Int = Parameters.dataOutInterval
  val waitTime:FiniteDuration = Parameters.waitTime
  val updateTime:FiniteDuration = Parameters.updateTime
  val commandUpdateTime:FiniteDuration = Parameters.commandUpdateTime
  val txPerBlock:Int = Parameters.txPerBlock
  var transactionFlag:Boolean = Parameters.transactionFlag
  var txProbability:Double = Parameters.txProbability
  val printFlag:Boolean = Parameters.printFlag
  val timingFlag:Boolean = Parameters.timingFlag
  val dataOutFlag:Boolean = Parameters.dataOutFlag
  val useFencing:Boolean = Parameters.useFencing
  val chainStoreInterval:Int = Parameters.chainStoreInterval
  val dataFileDir:String = Parameters.dataFileDir
  val refreshInterval:Int = Parameters.refreshInterval
  val stakeDistribution:String = Parameters.stakeDistribution
  val stakeScale:Double = Parameters.stakeScale
  val resourceScale:Double = Parameters.resourceScale
  val initStakeMin:Double = Parameters.initStakeMin
  val timeServer:String = Parameters.timeServer
  val maxBlockNumber:Int = Parameters.maxBlockNumber
  val simLabel:String = Parameters.simLabel
  val kesStoreInterval:Int = Parameters.kesStoreInterval
  val useStableIntervalTerm:Boolean = Parameters.useStableIntervalTerm
  val forging_window:Int = Parameters.forging_window

  val settings:AppSettings

  val localRef:ActorRefWrapper
  val holderIndex:Int
  val seed:Array[Byte]
  val serializer:Serializer
  val storageDir:String
  val localChain:Tine
  val walletStorage:WalletStorage
  val vrf:Vrf
  val kes:Kes
  val sig:Sig
  val fch:Fch
  val history:StateStorage
  val rng:Random
  val holderId:ActorPath
  val sessionId:Sid
  val phase:Double
  val chainStorage:ChainStorage
  val selfWrapper:ActorRefWrapper

  var password:String
  var derivedKey:Array[Byte]
  var salt:Array[Byte]
  var keyDir:String
  var wallet:Wallet
  var keys:Keys
  var keyFile:Option[KeyFile]
  var localState:State
  var eta:Eta
  var stakingState:State
  var memPool:MemPool
  var chainUpdateLock:Boolean
  var holders: List[ActorRefWrapper]
  var gOff:Int
  var numHello:Int
  var inbox:Map[Sid,(ActorRefWrapper,PublicKeys)]
  var blocksForged:Int
  var globalSlot:Slot
  var tinePool:Map[Int,(Tine,Int,Int,Int,ActorRefWrapper)]
  var tinePoolWithPrefix:Array[(Tine,Slot,Int)]
  var tineCounter:Int
  var genBlockHeader:BlockHeader
  var genBlockHash:Hash
  var roundBlock:Int
  var t0:Long
  var t1:Long
  var localSlot:Slot
  var currentEpoch:Int
  var updating:Boolean
  var actorStalled:Boolean
  var coordinatorRef:ActorRefWrapper
  var txCounter:Int
  var adversary:Boolean
  var covert:Boolean
  var forgeAll:Boolean

  var bootStrapLock:Boolean
  var helloLock:Boolean
  var bootStrapJob:Int
  var tineProvider:Option[ActorRefWrapper]
  var alphaCache:Option[LoadingCache[ByteArrayWrapper,Ratio]]
  var thresholdCache:Option[LoadingCache[(Ratio,Slot),Ratio]]
  var networkDelayList:List[Double]
  var tineLengthList:List[Double]
  var genesisBlock:Option[Block]

  def average(points:List[Double]):Double={
    val (net,num) = points.foldLeft((0.0,0))({ case ((s,l),x)=> (x+s,1+l) })
    net/num
  }
  def forgeBlock(forgerKeys:Keys):Unit
  def updateTine(inputTine:Tine):Option[(Tine,Slot)]
  def updateWallet():Unit
  def buildTine(job:(Int,(Tine,Int,Int,Int,ActorRefWrapper))):Unit
  def selectTine():Unit
  def bootstrapSelectTine():Unit
  def updateEpoch(slot:Slot,epochIn:Int,lastEta:Eta,chain:Tine,tine:Option[Tine]):(Int,Eta)
  def getStakingState(ep:Int,chain:Tine,tine:Option[Tine]):State
  def stakingTestStrategy(y:Rho,ps:Slot,bn:Int,rho:Rho,s_interval:Slot):Rho
  def update():Unit
  def scheduleDiffuse():Unit
  def Sha512(bytes: Array[Byte]):Array[Byte]
  def hash(input:ActorRefWrapper,slot:Slot, serializer: Serializer): Hash
  def hash(input:Slot,serializer: Serializer):Hash
  def hash(input:(ActorRefWrapper,PublicKeys), serializer: Serializer):Hash
  def hashGenEntry(input:(Array[Byte], ByteArrayWrapper, BigInt),serializer: Serializer):Hash
  def hash(input:BlockHeader,serializer: Serializer):Hash
  def hash(input:Transaction,serializer: Serializer):Hash
  def hash(input:(List[SlotId],Int,Int),serializer: Serializer):Hash
  def hashGen(input:GenesisSeq, serializer: Serializer):Hash
  def hash(input:TransactionSeq, serializer: Serializer):Hash
  def hash(input:String,serializer: Serializer):Hash
  def verifyTX(transaction: Transaction,sig:Sig,serializer: Serializer):Boolean
  def applyTransaction(t: Transaction,ls:State, forger:PublicKeyW, fee_r:Ratio):Option[State]
  def getParentId(b:BlockHeader):SlotId
  def phi(a:Ratio):Ratio
  def phi(a:Ratio,m_f:Ratio):Ratio
  def baseSlot(s:Slot):Slot
  def threshold_cached(a:Ratio, s_interval:Slot):Ratio
  def threshold(a:Ratio, s_interval:Slot):Ratio
  def factorial(n: Int):BigInt
  def compare(y: Array[Byte],t: Ratio):Boolean
  def relativeStake(holderKey:PublicKeyW,ls:State):Ratio
  def uuid:String
  def bytes2hex(b: Array[Byte]):String
  def hex2bytes(hex: String): Array[Byte]
  def getBlockHeader(bid:SlotId):Option[BlockHeader]
  def getParentBlockHeader(b:BlockHeader):Option[BlockHeader]
  def getParentId(bid:SlotId):Option[SlotId]
  def getNthParentId(bid:SlotId,n:Int):SlotId
  def getNonce(id:SlotId):Option[Rho]
  def eta_from_genesis(c:Tine, ep:Int):Eta
  def eta_from_tine(c:Tine,ep:Int,eta_prev:Eta,tine:Option[Tine]):Eta
  def gossipSet(self:ActorRefWrapper,holders:List[ActorRefWrapper]):List[ActorRefWrapper]
  def gossipSet(self:ActorRefWrapper,sender:ActorRefWrapper,holders:List[ActorRefWrapper]):List[ActorRefWrapper]
  def send(sender:ActorRefWrapper, holder:ActorRefWrapper, command: Any):Unit
  def send(sender:ActorRefWrapper, holders:List[ActorRefWrapper], command: Any):Unit
  def sendAssertDone(holders:List[ActorRefWrapper], command: Any):Unit
  def sendAssertDone(holder:ActorRefWrapper, command: Any):Unit
  def getStakingState(holder:ActorRefWrapper):State
  def blockTree(holder:ActorRefWrapper):Unit
  def getPositionData(router:ActorRefWrapper):(Map[ActorRefWrapper,(Double,Double)],Map[(ActorRefWrapper,ActorRefWrapper),Long])
  def verifyBlockHeader(b:BlockHeader):Boolean
  def verifyBlock(b:Block):Boolean
  def verifyChain(c:Tine, gh:Hash):Boolean
  def verifyTine(tine:Tine, prefix:Slot):Boolean
  def verifyTransaction(t:Transaction):Boolean
  def updateLocalState(ls:State, c:Tine):Option[State]
  def updateLocalState(ls:State, id:SlotId):Option[State]
  def trimMemPool():Unit
  def collectLedger(c:Tine):Unit
  def chooseLedger(pkw:PublicKeyW,mp:MemPool,s:State):TransactionSeq
  def timeFlag[R](block: => R):R
  def time[R](block: => R):R

}