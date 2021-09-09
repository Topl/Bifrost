package co.topl.stakeholder.components

import com.google.common.primitives.{Bytes, Ints, Longs}
import co.topl.stakeholder.primitives.ByteArrayWrapper
import co.topl.stakeholder.components
import co.topl.stakeholder.primitives._

import scala.collection.mutable
import scala.math.BigInt

//noinspection ScalaStyle

trait SerializationMethods extends SimpleTypes {
  import Serializer._
  import co.topl.stakeholder.remote.SpecTypes.{
    DiffuseDataType,
    HelloDataType,
    HoldersType,
    RequestBlockType,
    RequestTineType,
    ReturnBlocksType,
    SendBlockType,
    SendTxType
  }

  def sendTxFromBytes(bytes: Array[Byte]): SendTxType = {
    val msgBytes = new ByteStream(bytes, DeserializeSendTx)
    fromBytes(msgBytes) match {
      case msg: SendTxType @unchecked => msg
    }
  }

  def sendTxToBytes(msg: SendTxType): Array[Byte] = sSendTx(msg)

  def sendBlockFromBytes(bytes: Array[Byte]): SendBlockType = {
    val msgBytes = new ByteStream(bytes, DeserializeSendBlock)
    fromBytes(msgBytes) match {
      case msg: SendBlockType @unchecked => msg
    }
  }

  def sendBlockToBytes(msg: SendBlockType): Array[Byte] = sSendBlock(msg)

  def returnBlocksFromBytes(bytes: Array[Byte]): ReturnBlocksType = {
    val msgBytes = new ByteStream(bytes, DeserializeReturnBlocks)
    fromBytes(msgBytes) match {
      case msg: ReturnBlocksType @unchecked => msg
    }
  }

  def returnBlocksToBytes(msg: ReturnBlocksType): Array[Byte] = sReturnBlocks(msg)

  def requestTineFromBytes(bytes: Array[Byte]): RequestTineType = {
    val msgBytes = new ByteStream(bytes, DeserializeRequestTine)
    fromBytes(msgBytes) match {
      case msg: RequestTineType @unchecked => msg
    }
  }

  def requestTineToBytes(msg: RequestTineType): Array[Byte] = sRequestTine(msg)

  def requestBlockFromBytes(bytes: Array[Byte]): RequestBlockType = {
    val msgBytes = new ByteStream(bytes, DeserializeRequestBlock)
    fromBytes(msgBytes) match {
      case msg: RequestBlockType @unchecked => msg
    }
  }

  def requestBlockToBytes(msg: RequestBlockType): Array[Byte] = sRequestBlock(msg)

  def helloFromBytes(bytes: Array[Byte]): HelloDataType = {
    val msgBytes = new ByteStream(bytes, DeserializeHello)
    fromBytes(msgBytes) match {
      case msg: HelloDataType @unchecked => msg
    }
  }

  def helloToBytes(msg: HelloDataType): Array[Byte] = sHello(msg)

  def diffuseFromBytes(bytes: Array[Byte]): DiffuseDataType = {
    val msgBytes = new ByteStream(bytes, DeserializeDiffuse)
    fromBytes(msgBytes) match {
      case msg: DiffuseDataType @unchecked => msg
    }
  }

  def diffuseToBytes(msg: DiffuseDataType): Array[Byte] = sDiffuse(msg)

  def getBytes(bytes:        Array[Byte]): Array[Byte] = bytes
  def getBytes(int:          BigInt): Array[Byte] = sBigInt(int)
  def getBytes(int:          Int): Array[Byte] = Ints.toByteArray(int)
  def getBytes(long:         Long): Array[Byte] = Longs.toByteArray(long)
  def getBytes(bw:           ByteArrayWrapper): Array[Byte] = bw.data
  def getBytes(string:       String): Array[Byte] = sString(string)
  def getBytes(ratio:        Ratio): Array[Byte] = sRatio(ratio)
  def getBytes(slotId:       SlotId): Array[Byte] = Bytes.concat(getBytes(slotId._1), getBytes(slotId._2))
  def getBytes(cert:         Cert): Array[Byte] = sCert(cert)
  def getBytes(kesSignature: ForgingSignature): Array[Byte] = sKesSignature(kesSignature)
  def getBytes(state:        State): Array[Byte] = sState(state)
  def getBytes(transaction:  Transaction): Array[Byte] = sTransaction(transaction)
  def getBytes(mac:          Mac): Array[Byte] = sMac(mac)
  def getBytes(blockHeader:  BlockHeader): Array[Byte] = sBlockHeader(blockHeader)
  def getBytes(gen:          (Array[Byte], ByteArrayWrapper, BigInt)): Array[Byte] = sGen(gen)
  def getBytes(txs:          TransactionSeq): Array[Byte] = sTransactionSet(txs)
  def getBytes(idList:       List[BlockId]): Array[Byte] = sIdList(idList)
  def getBytes(bool:         Boolean): Array[Byte] = sBoolean(bool)
  def getBytes(chain:        Tine): Array[Byte] = sChain(chain)
  def getBytes(forgingKey:   ForgingKey): Array[Byte] = sForgingKey(forgingKey)
  def getBytes(block:        Block): Array[Byte] = sBlock(block)

  def getHoldersBytes(msg: HoldersType): Array[Byte] = sHolders(msg)
  def getGenesisBytes(txs: GenesisSeq): Array[Byte] = sGenesisSet(txs)

  def fromBytes(input: ByteStream): Any =
    input.caseObject match {
      case DeserializeBlockHeader       => dBlockHeader(input)
      case DeserializeMac               => dMac(input)
      case DeserializeTransaction       => dTransaction(input)
      case DeserializeGenesisSet        => dGenesisSet(input)
      case DeserializeTransactionSet    => dTransactionSet(input)
      case DeserializeIdList            => dIdList(input)
      case DeserializeState             => dState(input)
      case DeserializeChain             => dChain(input)
      case DeserializeForgingKey        => dForgingKey(input)
      case DeserializeBlock             => dBlock(input)
      case DeserializeGenesisBlock      => dBlock(input)
      case DeserializeDiffuse           => dDiffuse(input)
      case DeserializeHello             => dHello(input)
      case DeserializeRequestBlock      => dRequestBlock(input)
      case DeserializeRequestTine       => dRequestTine(input)
      case DeserializeReturnBlocks      => dReturnBlocks(input)
      case DeserializeSendBlock         => dSendBlock(input)
      case DeserializeSendTx            => dSendTx(input)
      case DeserializeHoldersFromRemote => dHolders(input)
    }

  private def sHolders(msg: HoldersType): Array[Byte] = {
    val out = Bytes.concat(msg._1.map(sString): _*)
    Ints.toByteArray(msg._1.length) ++ out ++ getBytes(msg._2) ++ getBytes(msg._3)
  }

  private def dHolders(stream: ByteStream): HoldersType = {
    val listLen = stream.getInt
    var out1: List[String] = List()
    var i = 0
    while (i < listLen) {
      val outLen = stream.getInt
      val outBytes = new ByteStream(stream.get(outLen), stream.caseObject)
      out1 ::= dString(outBytes)
      i += 1
    }
    val out2 = stream.get(pk_length)
    val out3 = stream.getLong
    assert(stream.empty)
    (out1, out2, out3)
  }

  private def sSendTx(msg: SendTxType): Array[Byte] =
    Bytes.concat(
      sString(msg._1),
      sString(msg._2),
      sTransaction(msg._3)
    )

  private def dSendTx(stream: ByteStream): SendTxType = {
    val strLen = stream.getInt
    val strBytes = new ByteStream(stream.get(strLen), stream.caseObject)
    val str = dString(strBytes)
    val strLen2 = stream.getInt
    val strBytes2 = new ByteStream(stream.get(strLen2), stream.caseObject)
    val str2 = dString(strBytes2)
    val txLen = stream.getInt
    val txBytes = new ByteStream(stream.get(txLen), stream.caseObject)
    val tx = dTransaction(txBytes)
    assert(stream.empty)
    (str, str2, tx)
  }

  private def sDiffuse(msg: DiffuseDataType): Array[Byte] =
    Bytes.concat(
      sString(msg._1),
      sString(msg._2),
      sString(msg._3),
      getBytes(msg._4),
      getBytes(msg._5._1),
      getBytes(msg._5._2),
      getBytes(msg._5._3)
    )

  private def dDiffuse(stream: ByteStream): DiffuseDataType = {
    val strLen1 = stream.getInt
    val strBytes1 = new ByteStream(stream.get(strLen1), stream.caseObject)
    val str1 = dString(strBytes1)
    val strLen2 = stream.getInt
    val strBytes2 = new ByteStream(stream.get(strLen2), stream.caseObject)
    val str2 = dString(strBytes2)
    val strLen3 = stream.getInt
    val strBytes3 = new ByteStream(stream.get(strLen3), stream.caseObject)
    val str3 = dString(strBytes3)
    val sid = ByteArrayWrapper(stream.get(hash_length))
    val pk1 = stream.get(pk_length)
    val pk2 = stream.get(pk_length)
    val pk3 = stream.get(pk_length)
    assert(stream.empty)
    (str1, str2, str3, sid, (pk1, pk2, pk3))
  }

  private def sHello(msg: HelloDataType): Array[Byte] =
    Bytes.concat(
      sString(msg._1),
      sString(msg._2),
      Ints.toByteArray(msg._3)
    )

  private def dHello(stream: ByteStream): HelloDataType = {
    val strLen = stream.getInt
    val strBytes = new ByteStream(stream.get(strLen), stream.caseObject)
    val str = dString(strBytes)
    val strLen2 = stream.getInt
    val strBytes2 = new ByteStream(stream.get(strLen2), stream.caseObject)
    val str2 = dString(strBytes2)
    val slot = stream.getInt
    assert(stream.empty)
    (str, str2, slot)
  }

  private def sRequestTine(msg: RequestTineType): Array[Byte] =
    Bytes.concat(
      sString(msg._1),
      sString(msg._2),
      getBytes(msg._3),
      getBytes(msg._4),
      getBytes(msg._5)
    )

  private def dRequestTine(stream: ByteStream): RequestTineType = {
    val strLen1 = stream.getInt
    val strBytes1 = new ByteStream(stream.get(strLen1), stream.caseObject)
    val str1 = dString(strBytes1)
    val strLen2 = stream.getInt
    val strBytes2 = new ByteStream(stream.get(strLen2), stream.caseObject)
    val str2 = dString(strBytes2)
    val slot = stream.getInt
    val blockId = ByteArrayWrapper(stream.get(id_length))
    val depth = stream.getInt
    val job = stream.getInt
    assert(stream.empty)
    (str1, str2, (slot, blockId), depth, job)
  }

  private def sRequestBlock(msg: RequestBlockType): Array[Byte] =
    Bytes.concat(
      sString(msg._1),
      sString(msg._2),
      getBytes(msg._3),
      getBytes(msg._4)
    )

  private def dRequestBlock(stream: ByteStream): RequestBlockType = {
    val strLen1 = stream.getInt
    val strBytes1 = new ByteStream(stream.get(strLen1), stream.caseObject)
    val str1 = dString(strBytes1)
    val strLen2 = stream.getInt
    val strBytes2 = new ByteStream(stream.get(strLen2), stream.caseObject)
    val str2 = dString(strBytes2)
    val slot = stream.getInt
    val blockId = ByteArrayWrapper(stream.get(id_length))
    val job = stream.getInt
    assert(stream.empty)
    (str1, str2, (slot, blockId), job)
  }

  private def sReturnBlocks(msg: ReturnBlocksType): Array[Byte] = {
    def mapFunc(b: Block): Array[Byte] = {
      val blockBytes = sBlock(b)
      getBytes(blockBytes.length) ++ blockBytes
    }
    Bytes.concat(
      sString(msg._1),
      sString(msg._2),
      getBytes(msg._3.length),
      Bytes.concat(msg._3.map(mapFunc): _*),
      getBytes(msg._4)
    )
  }

  private def dReturnBlocks(stream: ByteStream): ReturnBlocksType = {
    val strLen1 = stream.getInt
    val strBytes1 = new ByteStream(stream.get(strLen1), stream.caseObject)
    val str1 = dString(strBytes1)
    val strLen2 = stream.getInt
    val strBytes2 = new ByteStream(stream.get(strLen2), stream.caseObject)
    val str2 = dString(strBytes2)
    val numBlocks = stream.getInt
    var out: List[Block] = List()
    var i = 0
    while (i < numBlocks) {
      val outLen = stream.getInt
      val outBytes = new ByteStream(stream.get(outLen), DeserializeBlock)
      out = out ++ List(dBlock(outBytes))
      i += 1
    }
    val job = stream.getInt
    assert(out.length == numBlocks)
    assert(stream.empty)
    (str1, str2, out, job)
  }

  private def sSendBlock(msg: SendBlockType): Array[Byte] = {
    val blockBytes = sBlock(msg._3)
    Bytes.concat(
      sString(msg._1),
      sString(msg._2),
      getBytes(blockBytes.length),
      blockBytes
    )
  }

  private def dSendBlock(stream: ByteStream): SendBlockType = {
    val strLen = stream.getInt
    val strBytes = new ByteStream(stream.get(strLen), stream.caseObject)
    val str = dString(strBytes)
    val strLen2 = stream.getInt
    val strBytes2 = new ByteStream(stream.get(strLen2), stream.caseObject)
    val str2 = dString(strBytes2)
    val outLen = stream.getInt
    val outBytes = new ByteStream(stream.get(outLen), DeserializeBlock)
    val out = dBlock(outBytes)
    assert(stream.empty)
    (str, str2, out)
  }

  private def sBoolean(bool: Boolean): Array[Byte] =
    if (bool) {
      Ints.toByteArray(1)
    } else {
      Ints.toByteArray(0)
    }

  private def dBoolean(stream: ByteStream): Boolean = {
    val boolInt = stream.getInt
    assert(stream.empty && boolInt == 0 || boolInt == 1)
    boolInt match {
      case 0 => false
      case 1 => true
    }
  }

  private def sIdList(input: List[BlockId]): Array[Byte] = {
    def mapId(input: ByteArrayWrapper): Array[Byte] = if (input.data.isEmpty) {
      Array.fill[Byte](hash_length)(0.toByte)
    } else { input.data }
    Ints.toByteArray(input.length) ++ Bytes.concat(input.map(mapId): _*)
  }

  private def dIdList(stream: ByteStream): List[BlockId] = {
    val numEntries = stream.getInt
    var out: List[BlockId] = List()
    var i = 0
    val nullBytes = ByteArrayWrapper(Array.fill[Byte](hash_length)(0.toByte))
    while (i < numEntries) {
      val nextBytes = ByteArrayWrapper(stream.get(hash_length))
      if (nextBytes == nullBytes) {
        out = out ++ List(ByteArrayWrapper(Array()))
      } else {
        out = out ++ List(nextBytes)
      }
      i += 1
    }
    assert(out.length == numEntries)
    assert(stream.empty)
    out
  }

  private def sBigInt(int: BigInt): Array[Byte] = {
    val output = int.toByteArray
    Ints.toByteArray(output.length) ++ output
  }

  private def dBigInt(stream: ByteStream): BigInt = {
    val out = BigInt(stream.getAll)
    assert(stream.empty)
    out
  }

  private def sRatio(ratio: Ratio): Array[Byte] = {
    val out1 = sBigInt(ratio.numer)
    val out2 = sBigInt(ratio.denom)
    val output = Bytes.concat(out1, out2)
    Ints.toByteArray(output.length) ++ output
  }

  private def dRatio(stream: ByteStream): Ratio = {
    val out1len = stream.getInt
    val out1Bytes = new ByteStream(stream.get(out1len), Deserialize)
    val out1 = dBigInt(out1Bytes)
    val out2len = stream.getInt
    val out2Bytes = new ByteStream(stream.get(out2len), Deserialize)
    val out2 = dBigInt(out2Bytes)
    val out = new Ratio(
      out1,
      out2
    )
    assert(stream.empty)
    out
  }

  private def sString(string: String): Array[Byte] = {
    val output = string.getBytes
    Ints.toByteArray(output.length) ++ output
  }

  private def dString(stream: ByteStream): String = {
    val out = new String(stream.getAll)
    assert(stream.empty)
    out
  }

  private def sMac(mac: Mac): Array[Byte] = {
    val output = Bytes.concat(
      mac.hash.data,
      getBytes(mac.time)
    )
    output
  }

  private def dMac(stream: ByteStream): Mac = {
    val out = Mac(
      ByteArrayWrapper(stream.get(hash_length)),
      stream.getLong
    )
    assert(stream.empty)
    out
  }

  private def sTransaction(t: Transaction): Array[Byte] = {
    val output = Bytes.concat(
      t.sender.data,
      t.receiver.data,
      sBigInt(t.delta),
      t.sid.data,
      Ints.toByteArray(t.nonce),
      t.signature
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dTransaction(stream: ByteStream): Transaction = {
    val out1 = ByteArrayWrapper(stream.get(pkw_length))
    val out2 = ByteArrayWrapper(stream.get(pkw_length))
    val out3len = stream.getInt
    val out3Bytes = new ByteStream(stream.get(out3len), stream.caseObject)
    val out3 = dBigInt(out3Bytes)
    val out4 = ByteArrayWrapper(stream.get(sid_length))
    val out5 = stream.getInt
    val out6 = stream.get(sig_length)
    val out = Transaction(
      out1,
      out2,
      out3,
      out4,
      out5,
      out6
    )
    assert(stream.empty)
    out
  }

  private def sGen(gen: (Array[Byte], ByteArrayWrapper, BigInt)): Array[Byte] = {
    val output = Bytes.concat(
      gen._1,
      gen._2.data,
      sBigInt(gen._3)
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dGen(stream: ByteStream): (Array[Byte], ByteArrayWrapper, BigInt) = {
    val out1 = stream.get(hash_length)
    val out2 = ByteArrayWrapper(stream.get(pkw_length))
    val out3len = stream.getInt
    val out3Bytes = new ByteStream(stream.get(out3len), stream.caseObject)
    val out3 = dBigInt(out3Bytes)
    val out = (
      out1,
      out2,
      out3
    )
    assert(stream.empty)
    out
  }

  private def sBlockHeader(bh: BlockHeader): Array[Byte] =
    Bytes.concat(
      getBytes(bh._1),
      getBytes(bh._2),
      getBytes(bh._3),
      getBytes(bh._4),
      getBytes(bh._5),
      getBytes(bh._6),
      getBytes(bh._7),
      getBytes(bh._8),
      getBytes(bh._9),
      getBytes(bh._10)
    )

  private def dBlockHeader(stream: ByteStream): BlockHeader = {
    val out1 = ByteArrayWrapper(stream.get(hash_length))
    val out2 = ByteArrayWrapper(stream.get(hash_length))
    val out3 = stream.getInt
    val out4len = stream.getInt
    val out4Bytes = new ByteStream(stream.get(out4len), DeserializeBlockHeader)
    val out4 = dCert(out4Bytes)
    val out5 = stream.get(rho_length)
    val out6 = stream.get(pi_length)
    val out7len = stream.getInt
    val out7Bytes = new ByteStream(stream.get(out7len), DeserializeBlockHeader)
    val out7 = dKesSignature(out7Bytes)
    val out8 = stream.get(pk_length)
    val out9 = stream.getInt
    val out10 = stream.getInt
    val out = (
      out1,
      out2,
      out3,
      out4,
      out5,
      out6,
      out7,
      out8,
      out9,
      out10
    )
    assert(stream.empty)
    out
  }

  private def sCert(cert: Cert): Array[Byte] = {
    val output = Bytes.concat(
      getBytes(cert._1),
      getBytes(cert._2),
      getBytes(cert._3),
      getBytes(cert._4),
      getBytes(cert._5),
      getBytes(cert._6)
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dCert(stream: ByteStream): Cert = {
    val out1 = stream.get(pk_length)
    val out2 = stream.get(rho_length)
    val out3 = stream.get(pi_length)
    val out4 = stream.get(pk_length)
    val lenRatio = stream.getInt
    val ratioBytes = stream.get(lenRatio)
    val out5Bytes = new ByteStream(ratioBytes, DeserializeBlockHeader)
    val out5 = dRatio(out5Bytes)
    val lenString = stream.getInt
    val stringBytes = stream.get(lenString)
    val out6Bytes = new ByteStream(stringBytes, DeserializeBlockHeader)
    val out6 = dString(out6Bytes)
    val out = (
      out1,
      out2,
      out3,
      out4,
      out5,
      out6
    )
    assert(stream.empty)
    out
  }

  private def sKesSignature(kesSignature: ForgingSignature): Array[Byte] = {
    val output = Bytes.concat(
      Ints.toByteArray(kesSignature._1.length),
      kesSignature._1,
      Ints.toByteArray(kesSignature._2.length),
      kesSignature._2,
      Ints.toByteArray(kesSignature._3.length),
      kesSignature._3,
      getBytes(kesSignature._4),
      getBytes(kesSignature._5)
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dKesSignature(stream: ByteStream): ForgingSignature = {
    val out1len = stream.getInt
    val out1 = stream.get(out1len)
    val out2len = stream.getInt
    val out2 = stream.get(out2len)
    val out3len = stream.getInt
    val out3 = stream.get(out3len)
    val out4 = stream.getInt
    val out5 = stream.get(pk_length)
    val out = (
      out1,
      out2,
      out3,
      out4,
      out5
    )
    assert(stream.empty)
    out
  }

  private def sState(state: State): Array[Byte] = {
    def mapToBytes(in: (PublicKeyW, (BigInt, Boolean, Int))): Array[Byte] =
      in._1.data ++ getBytes(in._2._1) ++ getBytes(in._2._2) ++ getBytes(in._2._3)
    Ints.toByteArray(state.keySet.size) ++ Bytes.concat(state.toSeq.map(mapToBytes): _*)
  }

  private def dState(stream: ByteStream): State = {
    val numEntry = stream.getInt
    var out: State = Map()
    var i = 0
    while (i < numEntry) {
      val pkw = ByteArrayWrapper(stream.get(pkw_length))
      val biLen = stream.getInt
      val biBytes = new ByteStream(stream.get(biLen), stream.caseObject)
      val bi = dBigInt(biBytes)
      val boolBytes = new ByteStream(stream.get(4), stream.caseObject)
      val bool = dBoolean(boolBytes)
      val int = stream.getInt
      out += (pkw -> (bi, bool, int))
      i += 1
    }
    assert(out.keySet.size == numEntry)
    assert(stream.empty)
    out
  }

  private def sTransactionSet(sequence: TransactionSeq): Array[Byte] = {
    val bodyBytes = Bytes.concat(sequence.map(getBytes): _*)
    Ints.toByteArray(sequence.length) ++ bodyBytes
  }

  private def dTransactionSet(stream: ByteStream): TransactionSeq = {
    val numTx = stream.getInt
    var out: TransactionSeq = Seq()
    var i = 0
    while (i < numTx) {
      val outLen = stream.getInt
      val outBytes = new ByteStream(stream.get(outLen), stream.caseObject)
      out = out ++ Seq(dTransaction(outBytes))
      i += 1
    }
    assert(out.length == numTx)
    assert(stream.empty)
    out
  }

  private def sGenesisSet(sequence: GenesisSeq): Array[Byte] = {
    val output = Bytes.concat(sequence.map(getBytes): _*)
    Ints.toByteArray(sequence.length) ++ output
  }

  private def dGenesisSet(stream: ByteStream): GenesisSeq = {
    val numTx = stream.getInt
    var out: GenesisSeq = Seq()
    var i = 0
    while (i < numTx) {
      val outLen = stream.getInt
      val outBytes = new ByteStream(stream.get(outLen), stream.caseObject)
      out = out ++ Seq(dGen(outBytes))
      i += 1
    }
    assert(out.length == numTx)
    assert(stream.empty)
    out
  }

  private def sChain(tine: Tine): Array[Byte] = {
    def toBytes(in: (BigInt, SlotId)): Array[Byte] = Bytes.concat(getBytes(in._1.toInt), getBytes(in._2))
    val bestData = Bytes.concat(tine.best.toSeq.map(toBytes): _*)
    assert(bestData.nonEmpty)
    Ints.toByteArray(tine.best.keySet.size) ++
    bestData ++
    Ints.toByteArray(tine.minSlot.get) ++
    Ints.toByteArray(tine.maxSlot.get)
  }

  private def dChain(stream: ByteStream): TineData = {
    val numEntries = stream.getInt
    var out1: mutable.SortedMap[BigInt, SlotId] = mutable.SortedMap()
    var i = 0
    while (i < numEntries) {
      val index = stream.getInt
      val slot = stream.getInt
      val id: Hash = ByteArrayWrapper(stream.get(hash_length))
      out1 += (BigInt(index) -> (slot, id))
      i += 1
    }
    assert(out1.keySet.size == numEntries)
    val out2 = stream.getInt
    val out3 = stream.getInt
    assert(stream.empty)
    (out1, out2, out3)
  }

  private def sForgingKey(key: ForgingKey): Array[Byte] =
    Bytes.concat(
      sTree(key.L),
      sTree(key.Si),
      Ints.toByteArray(key.sig.length),
      key.sig,
      key.pki,
      key.rp,
      getBytes(key.offset)
    )

  private def dForgingKey(stream: ByteStream): ForgingKey = {
    val out1len = stream.getInt
    val out1Bytes = new ByteStream(stream.get(out1len), stream.caseObject)
    val out1 = dTree(out1Bytes)
    val out2len = stream.getInt
    val out2Bytes = new ByteStream(stream.get(out2len), stream.caseObject)
    val out2 = dTree(out2Bytes)
    val out3len = stream.getInt
    val out3 = stream.get(out3len)
    val out4 = stream.get(pk_length)
    val out5 = stream.get(hash_length)
    val out6 = stream.getInt
    assert(stream.empty)
    ForgingKey(out1, out2, out3, out4, out5, out6)
  }

  private def sTree(tree: Tree[Array[Byte]]): Array[Byte] = {
    def treeToBytes(t: Tree[Array[Byte]]): Array[Byte] =
      t match {
        case n: Node[Array[Byte]] =>
          n.l match {
            case Empty =>
              n.r match {
                case ll: Leaf[Array[Byte]] =>
                  Ints.toByteArray(2) ++ n.v ++ Ints.toByteArray(0) ++ ll.v
                case nn: Node[Array[Byte]] =>
                  Ints.toByteArray(2) ++ n.v ++ treeToBytes(nn)
              }
            case ll: Leaf[Array[Byte]] =>
              Ints.toByteArray(1) ++ n.v ++ Ints.toByteArray(0) ++ ll.v
            case nn: Node[Array[Byte]] =>
              Ints.toByteArray(1) ++ n.v ++ treeToBytes(nn)
          }
        case l: Leaf[Array[Byte]] =>
          Ints.toByteArray(0) ++ l.v
      }
    val output = treeToBytes(tree)
    Ints.toByteArray(output.length) ++ output
  }

  private def dTree(stream: ByteStream): Tree[Array[Byte]] = {
    def buildTree: Tree[Array[Byte]] =
      stream.getInt match {
        case 0 =>
          val bytes: Array[Byte] = stream.get(sig_length)
          Leaf(bytes)
        case 1 =>
          val bytes: Array[Byte] = stream.get(hash_length + sig_length)
          Node(bytes, buildTree, Empty)
        case 2 =>
          val bytes: Array[Byte] = stream.get(hash_length + sig_length)
          Node(bytes, Empty, buildTree)
      }
    val out = buildTree
    assert(stream.empty)
    out
  }

  private def sBlock(block: Block): Array[Byte] = {
    val idBytes = block.id.data
    val headerBytes = getBytes(block.tetraHeader)
    val bodyBytes = {
      if (block.slot == 0) {
        getGenesisBytes(block.genesisSet.get)
      } else {
        getBytes(block.blockBody.get)
      }
    }
    Bytes.concat(
      idBytes,
      Ints.toByteArray(headerBytes.length),
      headerBytes,
      Ints.toByteArray(bodyBytes.length),
      bodyBytes
    )
  }

  private def dBlock(stream: ByteStream): Block = {
    val id: ByteArrayWrapper = ByteArrayWrapper(stream.get(id_length))
    val headerLen = stream.getInt
    val headerBytes = new ByteStream(stream.get(headerLen), stream.caseObject)
    val header = Some(dBlockHeader(headerBytes))
    val bodyLen = stream.getInt
    val bodyBytes = new ByteStream(stream.get(bodyLen), stream.caseObject)
    stream.caseObject match {
      case DeserializeBlock =>
        val body = {
          if (!bodyBytes.empty) {
            Some(dTransactionSet(bodyBytes))
          } else {
            Some(Seq())
          }
        }
        assert(stream.empty)
        Block(id, header, body, None)
      case DeserializeGenesisBlock =>
        assert(stream.empty)
        Block(id, header, None, Some(dGenesisSet(bodyBytes)))
    }
  }

  private def sTxMap(txs: Map[Sid, Transaction]): Array[Byte] = {
    def mapBytes(entry: (Sid, Transaction)): Array[Byte] = getBytes(entry._1) ++ getBytes(entry._2)
    val output = Bytes.concat(
      txs.toSeq.map(mapBytes): _*
    )
    val total = Ints.toByteArray(txs.keySet.size) ++ output
    Ints.toByteArray(total.length) ++ total
  }

  private def dTxMap(stream: ByteStream): Map[Sid, Transaction] = {
    val numEntries = stream.getInt
    var out: Map[Sid, Transaction] = Map()
    var i = 0
    while (i < numEntries) {
      val sid: Sid = ByteArrayWrapper(stream.get(sid_length))
      val tx_len = stream.getInt
      val txBytes = new ByteStream(stream.get(tx_len), stream.caseObject)
      val tx: Transaction = dTransaction(txBytes)
      out += (sid -> tx)
      i += 1
    }
    assert(out.keySet.size == numEntries)
    assert(stream.empty)
    out
  }

}
