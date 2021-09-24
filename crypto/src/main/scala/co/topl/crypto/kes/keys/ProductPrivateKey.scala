package co.topl.crypto.kes.keys

import co.topl.models._
import co.topl.models.utility.{Empty, Leaf, Node, Tree}
import com.google.common.primitives.{Ints, Longs}

//abstract class ProductPrivateKey {
//  import ProductPrivateKey._
//  val data: KeyData
//  def update(globalTimeStep: Long): ProductPrivateKey
//  def sign(message:          Bytes): ProductSignature
//  def getVerificationKey: PublicKey
//  def timeStepPlusOffset: Long
//  def timeStep: Long
//  def getBytes: Bytes = serializer.getBytes(this)
//}

object ProductPrivateKey {

  case object DeserializeKey

  class Serializer {
    val pk_length: Int = 32
    val sig_length: Int = 64
    val hash_length: Int = 32

    def getBytes(key: KeyData): Bytes = sProductKey(key)

    def fromBytes(input: ByteStream): Any = dProductKey(input)

    private def sProductKey(key: KeyData): Bytes =
      Bytes.concat(
        sTree(key.superScheme),
        sTree(key.subScheme),
        Bytes(Ints.toByteArray(key.subSchemeSignature.length)),
        key.subSchemeSignature,
        key.subSchemePublicKey,
        key.subSchemeSeed,
        Bytes(Longs.toByteArray(key.offset))
      )

    private def dProductKey(stream: ByteStream): KeyData = {
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
      val out6 = stream.getLong
      assert(stream.empty)
      KeyData(out1, out2, out3, out4, out5, out6)
    }

    private def sTree(tree: Tree[Array[Byte]]): Bytes = {
      def treeToBytes(t: Tree[Array[Byte]]): Bytes =
        t match {
          case n: Node[Array[Byte]] =>
            n.l match {
              case Empty =>
                n.r match {
                  case ll: Leaf[Array[Byte]] =>
                    Bytes(Ints.toByteArray(2)) ++ n.v ++ Ints.toByteArray(0) ++ ll.v
                  case nn: Node[Array[Byte]] =>
                    Bytes(Ints.toByteArray(2)) ++ n.v ++ treeToBytes(nn)
                }
              case ll: Leaf[Array[Byte]] =>
                Bytes(Ints.toByteArray(1)) ++ n.v ++ Ints.toByteArray(0) ++ ll.v
              case nn: Node[Array[Byte]] =>
                Bytes(Ints.toByteArray(1)) ++ n.v ++ treeToBytes(nn)
            }
          case l: Leaf[Array[Byte]] =>
            Bytes(Ints.toByteArray(0)) ++ l.v
        }
      val output = treeToBytes(tree)
      Bytes(Ints.toByteArray(output.length)) ++ output
    }

    private def dTree(stream: ByteStream): Tree[Array[Byte]] = {
      def buildTree: Tree[Array[Byte]] =
        stream.getInt match {
          case 0 =>
            val bytes: Bytes = stream.get(sig_length)
            Leaf(bytes.toArray)
          case 1 =>
            val bytes: Bytes = stream.get(hash_length + sig_length)
            Node(bytes.toArray, buildTree, Empty)
          case 2 =>
            val bytes: Bytes = stream.get(hash_length + sig_length)
            Node(bytes.toArray, Empty, buildTree)
        }
      val out = buildTree
      assert(stream.empty)
      out
    }
  }

  class ByteStream(var data: Bytes, co: Any) {

    def get(n: Int): Bytes = {
      require(n <= data.length, "Error: ByteStream reached early end of stream")
      val out = data.take(n)
      data = data.drop(n)
      out
    }

    def getAll: Bytes = {
      val out = data
      data = Bytes(Array.emptyByteArray)
      out
    }

    def getInt: Int =
      Ints.fromByteArray(get(4).toArray)

    def getLong: Long =
      Longs.fromByteArray(get(8).toArray)
    def empty: Boolean = data.isEmpty
    def length: Int = data.length
    def caseObject: Any = co
  }

  val serializer: Serializer = new Serializer

  def deserializeProductKey(bytes: Bytes): KeyData = {
    val byteStream = new ByteStream(bytes, None)
    val numBytes = byteStream.getInt
    serializer.fromBytes(
      new ByteStream(byteStream.get(numBytes), DeserializeKey)
    ) match {
      case kd: KeyData =>
        require(byteStream.empty, "byte stream non-empty")
        kd
    }
  }

}
