package co.topl.attestation.keyManagement.wallets

import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA1Digest, SHA256Digest, SHA512Digest}
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.{ECDomainParameters, KeyParameter}
import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.ECNamedCurveParameterSpec
import org.bouncycastle.math.ec.ECPoint
import scodec.bits._

import java.io._
import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}
import java.security.{KeyFactory, Security}

/**
 * see https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
 */
object ByteVector32 {
  val Zeroes = ByteVector32(hex"0000000000000000000000000000000000000000000000000000000000000000")
  val One = ByteVector32(hex"0100000000000000000000000000000000000000000000000000000000000000")

  def fromValidHex(str: String) = ByteVector32(ByteVector.fromValidHex(str))

}

case class ByteVector32(bytes: ByteVector) {
  require(bytes.size == 32, s"size must be 32 bytes, is ${bytes.size} bytes")

  def reverse: ByteVector32 = ByteVector32(bytes.reverse)

  override def toString: String = bytes.toHex
}

object Bip32 {

  Security.addProvider(new BouncyCastleProvider())
  private val f = KeyFactory.getInstance("ECDSA", "BC");
  private val ecSpec = ECNamedCurveTable.getParameterSpec("secp256k1")
  private val ecDomain = new ECDomainParameters(ecSpec.getCurve, ecSpec.getG, ecSpec.getN)

  private val spec: ECNamedCurveParameterSpec = ECNamedCurveTable.getParameterSpec("secp256k1")

  case class KeyPath(path: Seq[Long]) {
    def lastChildNumber: Long = if (path.isEmpty) 0L else path.last

    def derive(number: Long) = KeyPath(path :+ number)

    override def toString = path.map(KeyPath.childNumberToString).foldLeft("m")(_ + "/" + _)
  }

  object KeyPath {
    val Root = KeyPath(Nil)

    /**
     * @param path key path. A list of integers separated by a `/`. May start with "/" or "m/". A single quote appended
     *             at the end means use the hardened version of the ley index (example: m/44'/0'/0'/0)
     * @return a KeyPath instance
     */
    def apply(path: String): KeyPath = {
      def toNumber(value: String): Long = if (value.last == '\'') hardened(value.dropRight(1).toLong) else value.toLong

      val path1 = path.stripPrefix("m").stripPrefix("/")
      if (path1.isEmpty) KeyPath.Root else new KeyPath(path1.split('/').map(toNumber).toSeq)
    }

    def childNumberToString(childNumber: Long) =
      if (isHardened(childNumber)) (childNumber - hardenedKeyIndex).toString + "'" else childNumber.toString
  }

  //implicit def keypath2longseq(input: KeyPath): Seq[Long] = input.path

  //implicit def longseq2keypath(input: Seq[Long]): KeyPath = KeyPath(input)

  val hardenedKeyIndex = 0x80000000L

  def hardened(index: Long): Long = hardenedKeyIndex + index

  def isHardened(index: Long): Boolean = index >= hardenedKeyIndex

  case class ExtendedPrivateKey(
    secretkeybytes: ByteVector32,
    chaincode:      ByteVector32,
    depth:          Int,
    path:           KeyPath,
    parent:         Long
  ) {

    def sk = new BigInteger(1, secretkeybytes.bytes.toArray)
    def skHex = secretkeybytes.toString

  }
  def bytes(input: InputStream, size: Long): ByteVector = bytes(input, size.toInt)

  def bytes(input: InputStream, size: Int): ByteVector = {
    val blob = new Array[Byte](size)
    if (size > 0) {
      val count = input.read(blob)
      if (count < size) throw new IOException("not enough data to read from")
    }
    ByteVector.view(blob)
  }

  def uint8(input: InputStream): Int = input.read()

  def uint32(input: InputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Long = {
    val bin = new Array[Byte](4)
    input.read(bin)
    uint32(bin, order)
  }

  def uint32(input: Array[Byte], order: ByteOrder): Long = {
    val buffer = ByteBuffer.wrap(input).order(order)
    buffer.getInt() & 0xffffffffL
  }

  def uint32(input: ByteVector, order: ByteOrder): Long =
    input.toLong(signed = false, ByteOrdering.fromJava(order))

  /*def encode(input: ExtendedPrivateKey, prefix: Int): String = {
        val out = new ByteArrayOutputStream()
        writeUInt8(input.depth, out)
        writeUInt32(input.parent.toInt, out, ByteOrder.BIG_ENDIAN)
        writeUInt32(input.path.lastChildNumber.toInt, out, ByteOrder.BIG_ENDIAN)
        out.write(input.chaincode.toArray)
        out.write(0)
        out.write(input.secretkeybytes.toArray)
        val buffer = ByteVector.view(out.toByteArray)
        Base58Check.encode(prefix, buffer)
  }*/

  case class ExtendedPublicKey(
    publickeybytes: ByteVector,
    chaincode:      ByteVector32,
    depth:          Int,
    path:           KeyPath,
    parent:         Long
  ) {
    require(publickeybytes.length == 33)
    require(chaincode.bytes.length == 32)

    def pk = new BigInteger(1, publickeybytes.toArray)
    def pkHex = publickeybytes.toHex

  }

  /*def encode(input: ExtendedPublicKey, prefix: Int): String = {
        val out = new ByteArrayOutputStream()
        write(input, out)
        val buffer = ByteVector.view(out.toByteArray)
        Base58Check.encode(prefix, buffer)
  }*/

  def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val mac = new HMac(new SHA512Digest())
    mac.init(new KeyParameter(key.toArray))
    mac.update(data.toArray, 0, data.length.toInt)
    val out = new Array[Byte](64)
    mac.doFinal(out, 0)
    ByteVector.view(out)
  }

  def getCompressed(pub: ECPoint): Array[Byte] = {
    val bb = ByteBuffer.allocate(33)
    bb.put((if (pub.getYCoord.testBitZero()) 0x03 else 0x02).toByte) // 3 if odd, 2 if even
    bb.put(pub.getXCoord().getEncoded())
    bb.array
  }

  /**
   * @param seed random seed
   * @return a "master" private key
   */
  def generate(seed: ByteVector): ExtendedPrivateKey = {
    val I = hmac512(ByteVector.view("Bitcoin seed".getBytes("UTF-8")), seed)
    val IL = ByteVector32(I.take(32))
    val IR = ByteVector32(I.takeRight(32))
    val path = new KeyPath(List.empty[Long])

    val privkey = new BigInteger(1, IL.bytes.toArray)

    val pointQ = spec.getG().multiply(privkey).normalize()
    val pKey = new BigInteger(1, getCompressed(pointQ)).toString(16)

    ExtendedPrivateKey(IL, IR, depth = 0, path, parent = 0L)
    //pointQ.getXCoord.toBigInteger.toString(16)

  }

  /**
   * @param input extended private key
   * @return the public key for this private key
   */
  def publicKey(input: ExtendedPrivateKey): ExtendedPublicKey = {

    val privkey = new BigInteger(1, input.secretkeybytes.bytes.toArray)

    val pointQ = spec.getG().multiply(privkey).normalize()
    val pKey = new BigInteger(1, getCompressed(pointQ))
    val pKeyHex = new BigInteger(1, getCompressed(pointQ)).toString(16)

    ExtendedPublicKey(
      ByteVector(pKey.toByteArray),
      input.chaincode,
      depth = input.depth,
      path = input.path,
      parent = input.parent
    )
  }

  def hash(digest: Digest)(input: ByteVector): ByteVector = {
    digest.update(input.toArray, 0, input.length.toInt)
    val out = new Array[Byte](digest.getDigestSize)
    digest.doFinal(out, 0)
    ByteVector.view(out)
  }

  def sha1 = hash(new SHA1Digest) _
  def sha256 = (x: ByteVector) => ByteVector32(hash(new SHA256Digest)(x))

  def ripemd160 = hash(new RIPEMD160Digest) _
  def hash160(input: ByteVector): ByteVector = ripemd160(sha256(input).bytes)

  /**
   * @param input extended public key
   * @return the fingerprint for this public key
   */
  def fingerprint(input: ExtendedPublicKey): Long = uint32(
    new ByteArrayInputStream(hash160(ByteVector(input.pk.toByteArray)).take(4).reverse.toArray)
  )

  /**
   * @param input extended private key
   * @return the fingerprint for this private key (which is based on the corresponding public key)
   */
  def fingerprint(input: ExtendedPrivateKey): Long = fingerprint(publicKey(input))

  def writeUInt32(input: Long, out: OutputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Unit =
    out.write(writeUInt32(input, order).toArray)

  def writeUInt32(input: Long, order: ByteOrder): ByteVector = {
    val bin = new Array[Byte](4)
    val buffer = ByteBuffer.wrap(bin).order(order)
    buffer.putInt((input & 0xffffffff).toInt)
    ByteVector.view(bin)
  }

  def writeUInt32(input: Long): ByteVector = writeUInt32(input, ByteOrder.LITTLE_ENDIAN)
  def fixSize(data:      ByteVector): ByteVector32 = ByteVector32(data.padLeft(32))

  /**
   * @param parent extended private key
   * @param index  index of the child key
   * @return the derived private key at the specified index
   */
  def derivePrivateKey(parent: ExtendedPrivateKey, index: Long): ExtendedPrivateKey = {
    val I = if (isHardened(index)) {
      val buffer = parent.secretkeybytes.bytes.+:(0.toByte)
      hmac512(parent.chaincode.bytes, buffer ++ writeUInt32(index.toInt, ByteOrder.BIG_ENDIAN))
    } else {
      val pub = publicKey(parent).publickeybytes
      hmac512(parent.chaincode.bytes, pub ++ writeUInt32(index.toInt, ByteOrder.BIG_ENDIAN))
    }
    val IL = ByteVector32(I.take(32))
    val IR = ByteVector32(I.takeRight(32))

    val bigIntKey = new BigInteger(1, IL.bytes.toArray)
    val bigIntParKey = new BigInteger(1, parent.secretkeybytes.bytes.toArray)
    //val N = Math.pow(2,252) + BigDecimal("27742317777372353535851937790883648493")

    if (bigIntKey.compareTo(ecSpec.getN()) >= 0) {
      throw new RuntimeException("cannot generated child private key")
    }

    //val privChildBytes = bigIntKey.add(bigIntParKey).mod(ecSpec.getN())
    val privChildBytes = bigIntKey.add(bigIntParKey).mod(ecSpec.getN())

    //val key = f.generatePrivate(new ECPrivateKeySpec(li, ecSpec)).asInstanceOf[ECPrivateKey]

    if (privChildBytes == 0) {
      throw new RuntimeException("cannot generated child private key")
    }

    val privChildBytes32 = fixSize(ByteVector.view(privChildBytes.toByteArray.dropWhile(_ == 0.toByte)))

    ExtendedPrivateKey(
      privChildBytes32,
      chaincode = IR,
      depth = parent.depth + 1,
      path = parent.path.derive(index),
      parent = fingerprint(parent)
    )
  }

  /**
   * @param parent extended public key
   * @param index  index of the child key
   * @return the derived public key at the specified index
   */
  def derivePublicKey(parent: ExtendedPublicKey, index: Long): ExtendedPublicKey = {
    require(!isHardened(index), "Cannot derive public keys from public hardened keys")

    val I = hmac512(parent.chaincode.bytes, parent.publickeybytes ++ writeUInt32(index.toInt, ByteOrder.BIG_ENDIAN))
    val IL = ByteVector32(I.take(32))
    val IR = ByteVector32(I.takeRight(32))

    val bigIntPubKey = new BigInteger(1, IL.bytes.toArray)
    //val bigIntParPubKey = new BigInteger(1, parent.publickeybytes.toArray)
    //val N = Math.pow(2,252) + BigDecimal("27742317777372353535851937790883648493")

    val ecPoint = ecSpec.getCurve.decodePoint(parent.publickeybytes.toArray)
    if (bigIntPubKey.compareTo(ecSpec.getN()) >= 0) {
      throw new RuntimeException("cannot generated child private key")
    }

    //val pubParKeyP = ecSpec.getG().multiply(bigIntParPubKey)
    val pubChildKeyPoint = ecSpec.getG().multiply(bigIntPubKey).add(ecPoint).normalize()

    if (pubChildKeyPoint.isInfinity) {
      throw new RuntimeException("cannot generated child public key")
    }
    val pubChildKey = new BigInteger(1, getCompressed(pubChildKeyPoint))

    ExtendedPublicKey(
      ByteVector(pubChildKey.toByteArray),
      chaincode = IR,
      depth = parent.depth + 1,
      path = parent.path.derive(index),
      parent = fingerprint(parent)
    )
  }

  //def derivePrivateKey(parent: ExtendedPrivateKey, chain: Seq[Long]): ExtendedPrivateKey = chain.foldLeft(parent)(derivePrivateKey)

  //def derivePrivateKey(parent: ExtendedPrivateKey, keyPath: KeyPath): ExtendedPrivateKey = derivePrivateKey(parent, keyPath.path)

  //def derivePublicKey(parent: ExtendedPublicKey, chain: Seq[Long]): ExtendedPublicKey = chain.foldLeft(parent)(derivePublicKey)

  //def derivePublicKey(parent: ExtendedPublicKey, keyPath: KeyPath): ExtendedPublicKey = derivePublicKey(parent, keyPath.path)

  // p2pkh mainnet
  val xprv = 0x0488ade4
  val xpub = 0x0488b21e

  // p2sh-of-p2wpkh mainnet
  val yprv = 0x049d7878
  val ypub = 0x049d7cb2

  // p2wpkh mainnet
  val zprv = 0x04b2430c
  val zpub = 0x04b24746

  // p2pkh testnet
  val tprv = 0x04358394
  val tpub = 0x043587cf

  // p2sh-of-p2wpkh testnet
  val uprv = 0x044a4e28
  val upub = 0x044a5262

  // p2wpkh testnet
  val vprv = 0x045f18bc
  val vpub = 0x045f1cf6
}
