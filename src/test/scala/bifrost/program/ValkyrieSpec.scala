package bifrost.program

import java.time.Instant

import InstrumentClasses.ProgramController
import InstrumentClasses.TokenClasses.AssetInstance
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.bifrostTransaction.AssetCreation
import bifrost.transaction.box.{AssetBox, BifrostBox}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import org.graalvm.polyglot.{Context, Instrument}
import bifrost.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.{Ints, Longs}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58

class ValkyrieSpec extends PropSpec
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  val testValkyrie: String =
    s"""
       |  issuer = 'b';
       |  issuer = '${publicKeys("investor")}';
       |  function create() {
       |    var toAddress = '${publicKeys("producer")}';
       |    res = Valkyrie_createAssets(issuer, toAddress, 10, 'testAssets', 0, '');
       |    a = 2 + 2; }
       |  function transferAssets() {
       |    var fromAddress = '${publicKeys("producer")}';
       |    var toAddress = '${publicKeys("hub")}';
       |    res = Valkyrie_transferAssets(issuer, fromAddress, toAddress, 10, 'testAssets', 0);}
       |  function Valkyrie_createAssets(issuer, to, amount, assetCode, fee, data) {
       |    res = ValkyrieReserved.createAssets(issuer, to , amount, assetCode, fee, data);
       |    return res; }
       |  function Valkyrie_transferAssets(issuer, from, to, amount, assetCode, fee) {
       |    res = ValkyrieReserved.transferAssets(issuer, from, to , amount, assetCode, fee);
       |    return res; };
     """.stripMargin



  property("Valkyrie function should generate new assetInstance") {


    val context: Context = Context
      .newBuilder("js")
      .option("Valkyrie", "true")
      .build

    val valkyrieController: ProgramController = ProgramController.find(context.getEngine) //context.getEngine.getInstruments.get("Valkyrie").lookup(classOf[ProgramController])


    assert(valkyrieController != null)

    context.eval("js", testValkyrie)

    context.eval("js", "create()")

    assert(context.getBindings("js").getMember("res").asBoolean())

    assert(valkyrieController.getNewAssetInstances.size == 1)

    val assetInstance: AssetInstance = valkyrieController.getNewAssetInstances.get(0)

    val proposition: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode(assetInstance.publicKey).get)
    val amount: Long = assetInstance.amount
    val assetCode: String = assetInstance.assetCode
    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode(assetInstance.issuer).get)
    val data: String = assetInstance.data

    val timestamp = Instant.now.toEpochMilli
    lazy val hashNoNonces = FastCryptographicHash(
        proposition.pubKeyBytes ++
        Longs.toByteArray(timestamp)
        //Longs.toByteArray(fee)
    )

    val nonce = AssetCreation.nonceFromDigest(FastCryptographicHash(
          "AssetCreation".getBytes ++
            proposition.pubKeyBytes ++
            issuer.pubKeyBytes ++
            assetCode.getBytes ++
            hashNoNonces ++
            Ints.toByteArray(0)
        ))

    val assetBox: AssetBox = AssetBox(proposition, nonce, amount, assetCode, issuer, data)

    assert(assetBox != null)
    assert(assetBox.proposition.pubKeyBytes sameElements Base58.decode(publicKeys("producer")).get)

  }

  property("Valkyrie function should transfer new assetInstance tp different public key") {


    val context: Context = Context
      .newBuilder("js")
      .option("Valkyrie", "true")
      .build

    val valkyrieController: ProgramController = ProgramController.find(context.getEngine) //context.getEngine.getInstruments.get("Valkyrie").lookup(classOf[ProgramController])


    assert(valkyrieController != null)

    context.eval("js", testValkyrie)

    context.eval("js", "create()")

    context.eval("js", "transferAssets()")

    assert(context.getBindings("js").getMember("res").asBoolean())

    assert(valkyrieController.getNewAssetInstances.size == 1)

    val assetInstance: AssetInstance = valkyrieController.getNewAssetInstances.get(0)

    val proposition: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode(assetInstance.publicKey).get)
    val amount: Long = assetInstance.amount
    val assetCode: String = assetInstance.assetCode
    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode(assetInstance.issuer).get)
    val data: String = assetInstance.data

    val timestamp = Instant.now.toEpochMilli
    lazy val hashNoNonces = FastCryptographicHash(
      proposition.pubKeyBytes ++
        Longs.toByteArray(timestamp)
      //Longs.toByteArray(fee)
    )

    val nonce = AssetCreation.nonceFromDigest(FastCryptographicHash(
      "AssetCreation".getBytes ++
        proposition.pubKeyBytes ++
        issuer.pubKeyBytes ++
        assetCode.getBytes ++
        hashNoNonces ++
        Ints.toByteArray(0)
    ))

    val assetBox: AssetBox = AssetBox(proposition, nonce, amount, assetCode, issuer, data)

    assert(assetBox != null)
    assert(assetBox.proposition.pubKeyBytes sameElements Base58.decode(publicKeys("hub")).get)


  }

}
