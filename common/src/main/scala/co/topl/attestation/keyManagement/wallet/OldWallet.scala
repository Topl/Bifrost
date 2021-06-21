//package co.topl.attestation.keyManagement
//
//import co.topl.crypto.hash.{hmacSha512, sha256, sha512}
//import co.topl.crypto.signatures.eddsa
//import io.estatico.newtype.macros.newtype
//import io.estatico.newtype.ops._
//import org.bouncycastle.crypto.Digest
//import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA512Digest}
//import org.bouncycastle.crypto.macs.HMac
//import org.bouncycastle.crypto.params.KeyParameter
//import scodec.bits._
//
//import java.io.{ByteArrayInputStream, InputStream, OutputStream}
//import java.math.BigInteger
//import java.nio.{ByteBuffer, ByteOrder}
//
///**
// * Wallet protocol based on SLIP-23: https://github.com/satoshilabs/slips/blob/master/slip-0023.md
// */
//package object wallet {
//
//  @newtype
//  class ByteVector32(val bytes: ByteVector) {
//    def reverse: ByteVector32 = ByteVector32(bytes.reverse)
//  }
//
//  object ByteVector32 {
//    def apply(bytes: ByteVector): ByteVector32 = {
//      require(bytes.size == 32, s"size must be 32 bytes, is ${bytes.size} bytes")
//
//      bytes.coerce
//    }
//
//    def fromHex(str: String): ByteVector32 = apply(ByteVector.fromValidHex(str))
//  }
//
//  private val hardenedKeyIndex = 0x80000000L
//
//  def hardened(index: Long): Long = hardenedKeyIndex + index
//
//  private def isHardened(index: Long): Boolean = index >= hardenedKeyIndex
//
//  case class KeyPath(path: Seq[Long]) {
//    def lastChildNumber: Long = if (path.isEmpty) 0L else path.last
//
//    def derive(number: Long) = KeyPath(path :+ number)
//
//    override def toString = path.map(KeyPath.childNumberToString).foldLeft("m")(_ + "/" + _)
//  }
//
//  object KeyPath {
//    val Root = KeyPath(Nil)
//
//    /**
//     * @param path key path. A list of integers separated by a `/`. May start with "/" or "m/". A single quote appended
//     *             at the end means use the hardened version of the ley index (example: m/44'/0'/0'/0)
//     * @return a KeyPath instance
//     */
//    def apply(path: String): KeyPath = {
//      def toNumber(value: String): Long = if (value.last == '\'') hardened(value.dropRight(1).toLong) else value.toLong
//
//      val path1 = path.stripPrefix("m").stripPrefix("/")
//      if (path1.isEmpty) KeyPath.Root else new KeyPath(path1.split('/').map(toNumber).toSeq)
//    }
//
//    def childNumberToString(childNumber: Long) =
//      if (isHardened(childNumber)) (childNumber - hardenedKeyIndex).toString + "'" else childNumber.toString
//  }
//
//  case class ExtendedPrivateKey(
//                                 secretKeyBytesLeft:  ByteVector32,
//                                 secretKeyBytesRight: ByteVector32,
//                                 chaincode:            ByteVector32,
//                                 depth:                Int,
//                                 path:                 KeyPath,
//                                 parent:               Long
//  ) {
//
//    def sk: BigInteger = new BigInteger(1, secretKeyBytesLeft.reverse.bytes.toArray)
//
//    def skHex: String = secretKeyBytesLeft.bytes.toHex
//
//  }
//
//  case class ExtendedPublicKey(
//    publickeybytes: ByteVector,
//    chaincode:      ByteVector32,
//    depth:          Int,
//    path:           KeyPath,
//    parent:         Long
//  ) {
//    //require(publickeybytes.length == 33)
//    require(chaincode.bytes.length == 32)
//
//    def pk = new BigInteger(1, publickeybytes.toArray)
//
//    def pkHex: String = publickeybytes.toHex
//
//  }
//
//  /**
//   * @param seed random seed
//   * @return a "master" private key
//   */
//  def generate(seed: ByteVector): ExtendedPrivateKey = {
//    val I = hmac512WithKey(ByteVector.view("ed25519 cardano seed".getBytes("UTF-8")), seed)
//    val IL = ByteVector32(I.take(32))
//    val chainCode = ByteVector32(I.takeRight(32))
//    val path = new KeyPath(List.empty[Long])
//    val k = sha512.hash(IL.bytes.toArray).value
//
//    k(0) = (k(0) & 0xf8.toByte).toByte
//    k(31) = ((k(31) & 0x1f.toByte) | 0x40.toByte).toByte
//
//    val kL = ByteVector32(ByteVector(k).take(32))
//
//    val kR = ByteVector32(ByteVector(k).takeRight(32))
//    ExtendedPrivateKey(kL, kR, chainCode, depth = 0, path, parent = 0L)
//  }
//
//  /**
//   * @param input extended private key
//   * @return the public key for this private key
//   */
//  def publicKey(input: ExtendedPrivateKey): ExtendedPublicKey = {
//    val privkey = input.secretKeyBytesLeft.bytes
//
//    val ed = new eddsa.Ed25519
//    val sk = privkey
//    val pk = new Array[Byte](ed.PUBLIC_KEY_SIZE)
//
//    ed.scalarMultBaseEncoded(privkey.toArray, pk, 0)
//
//    ExtendedPublicKey(ByteVector(pk), input.chaincode, depth = input.depth, path = input.path, parent = input.parent)
//  }
//
//  /**
//   * @param parent extended private key
//   * @param index  index of the child key
//   * @return the derived private key at the specified index
//   */
//  def derivePrivateKey(parent: ExtendedPrivateKey, index: Long): ExtendedPrivateKey = {
//    val I = if (isHardened(index)) {
//      val buffer = parent.secretKeyBytesLeft.bytes ++ parent.secretKeyBytesRight.bytes.+:(0.toByte)
//      hmac512WithKey(parent.chaincode.bytes, buffer ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN))
//    } else {
//      hmac512WithKey(
//        parent.chaincode.bytes,
//        publicKey(parent).publickeybytes.+:(0x02.toByte) ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN)
//      )
//    }
//
//    val IL = ByteVector(I.take(28).toArray)
//    val IR = ByteVector32(I.takeRight(32))
//    val bi = BigInteger.valueOf(8)
//
//    val N = BigInteger.valueOf(2).pow(256)
//    val bigIntKeyLeft = new BigInteger(1, (IL.toArray))
//    val bigIntKeyRight = new BigInteger(1, IR.bytes.toArray)
//    val bigIntParKeyLeft = new BigInteger(1, parent.secretKeyBytesLeft.bytes.toArray)
//    val bigIntParKeyRight = new BigInteger(1, parent.secretKeyBytesRight.bytes.toArray)
//    val privChildLeft = bigIntKeyLeft.multiply(bi).add(bigIntParKeyLeft)
//    val privChildRight = bigIntKeyRight.add(bigIntParKeyRight).mod(N)
//    val privChildLeftBytes32 = fixSize(ByteVector.view(privChildLeft.toByteArray.dropWhile(_ == 0.toByte)))
//    val privChildRightBytes32 = fixSize(ByteVector.view(privChildRight.toByteArray.dropWhile(_ == 0.toByte)))
//    val IC = if (isHardened(index)) {
//      val buffer = parent.secretKeyBytesLeft.bytes ++ parent.secretKeyBytesRight.bytes.+:(0x01.toByte)
//      hmac512WithKey(parent.chaincode.bytes, buffer ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN))
//    } else {
//      val pub = publicKey(parent).publickeybytes.+:(0x03.toByte)
//      hmac512WithKey(parent.chaincode.bytes, pub ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN))
//    }
//
//    val ICR = ByteVector32(IC.takeRight(32))
//
//    ExtendedPrivateKey(
//      privChildLeftBytes32,
//      privChildRightBytes32,
//      chaincode = ICR,
//      depth = parent.depth + 1,
//      path = parent.path.derive(index),
//      parent = fingerprint(parent)
//    )
//  }
//
//  /**
//   * @param parent extended public key
//   * @param index  index of the child key
//   * @return the derived public key at the specified index
//   */
//  def derivePublicKey(parent: ExtendedPublicKey, index: Long, parent_priv: ExtendedPrivateKey): ExtendedPublicKey = {
//    require(!isHardened(index), "Cannot derive public keys from public hardened keys")
//
//    val I = hmac512WithKey(
//      parent.chaincode.bytes,
//      parent.publickeybytes.+:(0x02.toByte) ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN)
//    )
//    val IL = ByteVector(I.take(28).toArray)
//    val IR = ByteVector32(I.takeRight(32))
//    val N = BigInteger.valueOf(2).pow(256)
//
//    val bi = BigInteger.valueOf(8)
//
//    val bigIntParKeyLeft = new BigInteger(1, parent_priv.secretKeyBytesLeft.bytes.toArray)
//    val bigIntKeyLeft = new BigInteger(1, IL.toArray)
//    val bigIntKeyRight = new BigInteger(1, IR.bytes.toArray)
//    val ChildLeft = bigIntKeyLeft.multiply(bi)
//
//    val ChildLeftBytes32 = fixSize(ByteVector.view(ChildLeft.toByteArray.dropWhile(_ == 0.toByte)))
//
//    val ed = new eddsa.Ed25519
//
//    //val pubParKeyP = ecSpec.getG().multiply(bigIntParPubKey)
//    val pkLeft = new Array[Byte](ed.PUBLIC_KEY_SIZE)
//    val s = new Array[Byte](32)
//
//    //pruneScalar(ChildLeftBytes32.bytes.toArray, 0, s)
//
//    val p = new ed.PointAccum
//    ed.scalarMultBase(ChildLeftBytes32.bytes.toArray, p)
//
//    val pA = new ed.PointExt
//    ed.decodePointVar(parent.publickeybytes.toArray, 0, true, pA)
//
//    //pointAddVar(true,pA, p)
//
//    val pubChildKey = new Array[Byte](32)
//    val temp = new Array[Byte](32)
//    //pubChildKey(0) = 0
//
//    ed.scalarMultBaseEncoded(ChildLeftBytes32.bytes.toArray, temp, 0)
//
//    val pB = new ed.PointExt
//    val sum = new ed.PointExt
//    //decodePointVar(pubChildKey, 0, true, pB)
//
//    ed.pointAddVar(true, pA, p)
//    println("Public POINT" + p)
//    ed.encodePoint(p, pubChildKey, 0)
//
//    val IC = hmac512WithKey(
//      parent.chaincode.bytes,
//      parent.publickeybytes.+:(0x03.toByte) ++ writeUInt32(index.toInt, ByteOrder.LITTLE_ENDIAN)
//    )
//
//    val ICR = ByteVector32(IC.takeRight(32))
//
//    ExtendedPublicKey(
//      ByteVector(pubChildKey),
//      chaincode = ICR,
//      depth = parent.depth + 1,
//      path = parent.path.derive(index),
//      parent = fingerprint(parent)
//    )
//  }
//
//  /**
//   * @param input extended public key
//   * @return the fingerprint for this public key
//   */
//  private def fingerprint(input: ExtendedPublicKey): Long = uint32(
//    new ByteArrayInputStream(hash160(ByteVector(input.pk.toByteArray)).take(4).reverse.toArray)
//  )
//
//  /**
//   * @param input extended private key
//   * @return the fingerprint for this private key (which is based on the corresponding public key)
//   */
//  private def fingerprint(input: ExtendedPrivateKey): Long = fingerprint(publicKey(input))
//
//  private def hmac512WithKey(key: ByteVector, data: ByteVector): ByteVector = {
//    val mac = new HMac(new SHA512Digest())
//    mac.init(new KeyParameter(key.toArray))
//    mac.update(data.toArray, 0, data.length.toInt)
//    val out = new Array[Byte](64)
//    mac.doFinal(out, 0)
//    ByteVector.view(out)
//  }
//
//  private def writeUInt32(input: Long, order: ByteOrder): ByteVector = {
//    val bin = new Array[Byte](4)
//    val buffer = ByteBuffer.wrap(bin).order(order)
//    buffer.putInt((input & 0xffffffff).toInt)
//    ByteVector.view(bin)
//  }
//
//  private def fixSize(data: ByteVector): ByteVector32 = ByteVector32(data.padLeft(32))
//
//  private def hash(digest: Digest)(input: ByteVector): ByteVector = {
//    digest.update(input.toArray, 0, input.length.toInt)
//    val out = new Array[Byte](digest.getDigestSize)
//    digest.doFinal(out, 0)
//    ByteVector.view(out)
//  }
//
//  private def ripemd160 = hash(new RIPEMD160Digest) _
//  private def hash160(input: ByteVector): ByteVector = ripemd160(ByteVector(sha256.hash(input.toArray).value))
//
//  private def uint32(input: InputStream, order: ByteOrder = ByteOrder.LITTLE_ENDIAN): Long = {
//    val bin = new Array[Byte](4)
//    input.read(bin)
//    uint32(bin, order)
//  }
//
//  private def uint32(input: Array[Byte], order: ByteOrder): Long = {
//    val buffer = ByteBuffer.wrap(input).order(order)
//    buffer.getInt() & 0xffffffffL
//  }
//}
