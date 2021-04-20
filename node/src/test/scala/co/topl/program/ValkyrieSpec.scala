package co.topl.program

import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.DoNotDiscover
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

@DoNotDiscover
class ValkyrieSpec extends AnyPropSpec
  with Matchers
  with CoreGenerators
  with ValidGenerators {

/*
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
       |  function transferArbits() {
       |    var fromAddress = '${publicKeys("investor")}';
       |    var toAddress = '${publicKeys("hub")}';
       |    res = Valkyrie_transferArbits(fromAddress, toAddress, 10, 0);}
       |  function Valkyrie_createAssets(issuer, to, amount, assetCode, fee, data) {
       |    res = ValkyrieReserved.createAssets(issuer, to , amount, assetCode, fee, data);
       |    return res; }
       |  function Valkyrie_transferAssets(issuer, from, to, amount, assetCode, fee) {
       |    res = ValkyrieReserved.transferAssets(issuer, from, to , amount, assetCode, fee);
       |    return res; };
       |  function Valkyrie_transferArbits(from, to, amount, fee) {
       |    res = ValkyrieReserved.transferArbits(from, to , amount, fee);
       |    return res; };
     """.stripMargin


  property("Valkyrie function should generate new assetInstance") {

    val context: Context = Context
      .newBuilder("js")
      .option("Valkyrie", "true")
      .build

    val valkyrieController: ProgramController = ProgramController.find(context.getEngine) //context.getEngine.getInstruments.get("Valkyrie").lookup(classOf[ProgramController])

    //println(s"${Class.forName("com/oracle/truffle/api/instrumentation/TruffleInstrument").toString}")

    assert(valkyrieController != null)

    context.eval("js", testValkyrie)

    context.eval("js", "create()")

    assert(context.getBindings("js").getMember("res").asBoolean())

    assert(valkyrieController.getNewAssetInstances.size == 1)

    val assetInstance: AssetInstance = valkyrieController.getNewAssetInstances.get(0)

    val proposition: PublicKeyPropositionCurve25519 = PublicKey25519Proposition(assetInstance.publicKey)
    val amount: Long = assetInstance.amount
    val assetCode: String = assetInstance.assetCode
    val issuer: PublicKeyPropositionCurve25519 = PublicKey25519Proposition(assetInstance.issuer)
    val data: String = assetInstance.data

    val timestamp = Instant.now.toEpochMilli
    lazy val hashNoNonces = Blake2b256(
      proposition.pubKeyBytes ++
        Longs.toByteArray(timestamp)
      //Longs.toByteArray(fee)
    )

    val nonce = Transaction.nonceFromDigest(Blake2b256(
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

  property("Valkyrie function should transfer new assetInstance to different public key") {


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

    val proposition: PublicKeyPropositionCurve25519 = PublicKey25519Proposition(assetInstance.publicKey)
    val amount: Long = assetInstance.amount
    val assetCode: String = assetInstance.assetCode
    val issuer: PublicKeyPropositionCurve25519 = PublicKey25519Proposition(assetInstance.issuer)
    val data: String = assetInstance.data

    val timestamp = Instant.now.toEpochMilli
    lazy val hashNoNonces = Blake2b256(
      proposition.pubKeyBytes ++
        Longs.toByteArray(timestamp)
      //Longs.toByteArray(fee)
    )

    val nonce = Transaction.nonceFromDigest(Blake2b256(
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

  property("Valkyrie function should transfer inputted arbit box to different public key") {

    val context: Context = Context
      .newBuilder("js")
      .option("Valkyrie", "true")
      .build

    val valkyrieController: ProgramController = ProgramController.find(context.getEngine) //context.getEngine.getInstruments.get("Valkyrie").lookup(classOf[ProgramController])

    assert(valkyrieController != null)

    val state: State = State.readOrGenerate(settings)

    assert(state.getTokenBoxes(PublicKey25519Proposition(publicKeys("investor"))).nonEmpty)

    val arbitInstances: util.ArrayList[ArbitInstance] = new util.ArrayList()

    //Sanitize inputBoxes
    state.getTokenBoxes(PublicKey25519Proposition(publicKeys("investor"))).getOrElse(Seq()).foreach {
      case arbitBox: ArbitBox =>
        arbitInstances.add(new ArbitInstance(Base58.encode(arbitBox.proposition.pubKeyBytes), arbitBox.value, arbitBox.id.hashBytes))
      case _ =>
    }

    valkyrieController.setArbitBoxesForUse(arbitInstances)

    context.eval("js", testValkyrie)

    context.eval("js", "transferArbits()")

    assert(context.getBindings("js").getMember("res").asBoolean())

    //Two new boxes should be outputted after transfer
    assert(valkyrieController.getNewArbitInstances.size == 2)

    //One box should be removed from list of input boxes
    assert(valkyrieController.getBoxesToRemove.get(0) sameElements(arbitInstances.get(0).boxId))

    //Parsing the new arbit instance as an arbit box
    val newArbitInstance1: ArbitInstance = valkyrieController.getNewArbitInstances.get(0)

    val proposition: PublicKeyPropositionCurve25519 = PublicKey25519Proposition(newArbitInstance1.publicKey)
    val amount: Long = newArbitInstance1.amount

    val timestamp = Instant.now.toEpochMilli

    lazy val hashNoNonces = Blake2b256(
      proposition.pubKeyBytes) ++
      //unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp)
    //Longs.toByteArray(fee)

    val nonce = Transaction
      .nonceFromDigest(Blake2b256("ArbitTransfer".getBytes
        ++ proposition.pubKeyBytes
        ++ hashNoNonces
        ++ Ints.toByteArray(0)))

    val newArbitBox1: ArbitBox = ArbitBox(proposition, nonce, amount)

    assert(newArbitBox1 != null)
    assert(newArbitBox1.proposition.pubKeyBytes sameElements Base58.decode(publicKeys("hub")).get)
    assert(newArbitBox1.value == 10)

  }
*/
}
