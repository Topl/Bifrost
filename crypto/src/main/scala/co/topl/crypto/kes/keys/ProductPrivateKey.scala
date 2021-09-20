package co.topl.crypto.kes.keys

import co.topl.crypto.kes.construction.KeyData
import co.topl.crypto.kes.signatures.ProductSignature
import com.google.common.primitives.{Bytes, Ints, Longs}
import co.topl.crypto.PublicKey

abstract class ProductPrivateKey {
  import ProductPrivateKey._
  val data: KeyData
  def update(globalTimeStep: Long): ProductPrivateKey
  def sign(message:          Array[Byte]): ProductSignature
  def getVerificationKey: PublicKey
  def timeStepPlusOffset: Long
  def timeStep: Long
  def getBytes: Array[Byte] = serializer.getBytes(this)
}

object ProductPrivateKey {

  case object DeserializeKey

  class Serializer {
    import co.topl.crypto.kes.construction.{Empty, Leaf, Node, Tree}
    val pk_length: Int = 32
    val sig_length: Int = 64
    val hash_length: Int = 32

    def getBytes(key: ProductPrivateKey): Array[Byte] = sProductKey(key.data)
    def getBytes(key: KeyData): Array[Byte] = sProductKey(key)

    def fromBytes(input: ByteStream): Any = dProductKey(input)

    private def sProductKey(key: KeyData): Array[Byte] =
      Bytes.concat(
        sTree(key.superScheme),
        sTree(key.subScheme),
        Ints.toByteArray(key.subSchemeSignature.length),
        key.subSchemeSignature,
        key.subSchemePublicKey,
        key.subSchemeSeed,
        Longs.toByteArray(key.offset)
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
  }

  class ByteStream(var data: Array[Byte], co: Any) {

    def get(n: Int): Array[Byte] = {
      require(n <= data.length, "Error: ByteStream reached early end of stream")
      val out = data.take(n)
      data = data.drop(n)
      out
    }

    def getAll: Array[Byte] = {
      val out = data
      data = Array()
      out
    }

    def getInt: Int =
      Ints.fromByteArray(get(4))

    def getLong: Long =
      Longs.fromByteArray(get(8))
    def empty: Boolean = data.isEmpty
    def length: Int = data.length
    def caseObject: Any = co
  }

  val serializer: Serializer = new Serializer

  def deserializeProductKey(bytes: Array[Byte]): KeyData = {
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
