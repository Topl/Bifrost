package bifrost.program

import bifrost.exceptions.{InvalidProvidedProgramArgumentsException, JsonParsingException}
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.srb.StateBoxRegistry
import bifrost.transaction.box.{CodeBox, StateBox}
import io.circe._
import io.circe.syntax._
import org.graalvm.polyglot.Context
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import io.iohk.iodb.LSMStore
import scorex.crypto.encode.Base58

import scala.util.Try
import scala.language.existentials

/**
  *
  * @param parties            Public keys allowed to interact with the program
  * @param lastUpdated        timestamp of last update made to the program
  * @param id                 Unique identifier
  * @param executionBuilder   Context for the state and code to execute methods on the program
  */
case class Program(parties: Map[PublicKey25519Proposition, String],
                   lastUpdated: Long,
                   id: Array[Byte],
                   executionBuilder: Json) {

  val MIN_PARTIES: Int = 2
  val MAX_PARTIES: Int = 1024

  if (parties.size < MIN_PARTIES || parties.size > MAX_PARTIES) {
    throw new InvalidProvidedProgramArgumentsException("An invalid number of parties was specified for the program " +
      "(must be between 2 and 1024).")
  }

  val jsre: Context = Context.create("js")


  val executionBuilderObj: ExecutionBuilder = executionBuilder.as[ExecutionBuilder] match {
    case Right(a) => a
    case Left(_) => throw new JsonParsingException("Was unable to parse a valid executionBuilder from provided JSON")
  }

  jsre.eval("js", ProgramPreprocessor.objectAssignPolyfill)

  //noinspection ScalaStyle
  def applyFunction(methodName: String)(args: JsonObject)(params: Array[String])/*: Try[(Program, Option[Json])]*/ = Try {



    }

  lazy val json: Json = Map(
    "executionBuilder" -> executionBuilder,
    "parties" -> parties
      .map(p => {
        Base58.encode(p._1.pubKeyBytes) -> p._2.asJson
      })
      .asJson,
    "lastUpdated" -> lastUpdated.asJson,
    "id" -> Base58.encode(id).asJson
  ).asJson

}

object Program {

  def apply(programJson: Json, id: Array[Byte]): Program = {
    val jsonMap: Map[String, Json] = programJson
      .asObject
      .map(_.toMap)
      .get

    val parties: Map[PublicKey25519Proposition, String] = jsonMap("parties").asObject match {
      case Some(partiesObject) =>
        partiesObject
          .toMap
          .map {
            party =>
              val publicKey = Base58.decode(party._1).get
              val role = party._2.asString.get
              new PublicKey25519Proposition(publicKey) -> role
          }
      case None => throw new JsonParsingException(s"Error: ${jsonMap("parties")}")
    }

    new Program(
      parties, // TODO #22 new PublicKey25519Proposition(Base58.decode(jsonMap("producer").asString.get).get),
      jsonMap("lastUpdated").asNumber.get.toLong.getOrElse(0L),
      id,
      jsonMap("executionBuilder")
    )
  }

  //noinspection ScalaStyle
  /**
    *
    * @param stateBoxes   Set of StateBoxes to form program
    * @param codeBoxes    Set of CodeBoxes to form program
    * @param methodName   The method to be called on the program
    * @param party        Public key making the call
    * @param args         parameters for the method
    * @return             State members to update the StateBox(es)
    */
  def execute(stateBoxes: Seq[StateBox], codeBoxes: Seq[CodeBox], methodName: String)
             (party: PublicKey25519Proposition)
             (args: JsonObject): Json /*: Try[Either[Program, Json]]*/ = /*Try*/ {

    val mutableState: Seq[(String, String)] = stateBoxes.head.value.as[Map[String, String]].toSeq.flatten
    val state: Seq[(String, String)] = stateBoxes.flatMap(sb => sb.value.as[Map[String, String]].toSeq.flatten)
    val programCode: String = codeBoxes.foldLeft("")((a,b) => a ++ b.value.foldLeft("")((a,b) => a ++ (b + "\n")))

    // Create new execution context and evaluate method
    val jsre: Context = Context.create("js")
    val bindings = jsre.getBindings("js")

    mutableState.foreach(s => bindings.putMember(s._1, s._2))
    jsre.eval("js", programCode)

    //TODO Sanitize JS method creation
    val params = args.values.foldLeft("")((a,b) => a + "," + b.toString)
    val methodJS: String = methodName + "(" + params + ")"

    val methodEval = jsre.eval("js", methodJS)

    val output: Map[String, String] = mutableState.map(s => s._1 -> bindings.getMember(s._1).toString).toMap

    output.asJson
  }

  // TODO Fix instantiation to handle runtime input and/or extract to a better location
  val forgingSettings = new ForgingSettings {
    override def settingsJSON: Map[String, Json] = super.settingsFromFile("testSettings.json")
  }

  val sbr: StateBoxRegistry = StateBoxRegistry.readOrGenerate(forgingSettings)
  val storage: BifrostHistory = BifrostHistory.readOrGenerate(forgingSettings)

  //def getStatebox(): StateBox =
}