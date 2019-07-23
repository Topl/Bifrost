package bifrost.program

import java.util.UUID

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.exceptions.{InvalidProvidedProgramArgumentsException, JsonParsingException}
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.srb.StateBoxRegistry
import bifrost.transaction.box.{CodeBox, StateBox}
import io.circe._
import io.circe.syntax._
import io.circe.parser._
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


  /**
    *
    * @param stateBoxes   Set of StateBoxes to form program
    * @param codeBoxes    Set of CodeBoxes to form program
    * @param methodName   The method to be called on the program
    * @param party        Public key making the call
    * @param args         parameters for the method
    * @return             State members to update the mutable StateBox
    */
  //noinspection ScalaStyle
  def execute(stateBoxes: Seq[(StateBox, UUID)], codeBoxes: Seq[CodeBox], methodName: String)
             (party: PublicKey25519Proposition)
             (args: JsonObject): Json = {

    val mutableState = stateBoxes.head._1.state.asObject.get.toMap
    val programCode: String = codeBoxes.foldLeft("")((a,b) => a ++ b.code.foldLeft("")((a,b) => a ++ (b + "\n")))

    val jsre: Context = Context.create("js")
    val bindings = jsre.getBindings("js")

    //Pass in JSON objects for each read-only StateBox
    stateBoxes.tail.map{ sb =>
      val formattedUuid: String = "_" + sb._2.toString.replace("-", "_")
      jsre.eval("js", s"""var $formattedUuid = JSON.parse(${sb._1.state})""")
    }

    //Inject function to read from read only StateBoxes
    val getFromState =
      s"""
         |function getFromState(uuid, value) {
         |  return this[uuid][value]
         |}
       """.stripMargin

    jsre.eval("js", getFromState)

    //Pass in writable state and functions
    mutableState.foreach{s =>
      jsre.eval("js", s"${s._1} = ${s._2}")
    }
    jsre.eval("js", programCode)
    println(s"Right after injecting function code, a = ${bindings.getMember("a")}")

    val params: Array[String] = args.toMap.values.toArray.map(_.noSpaces)
    val paramString: String = if(params.nonEmpty) {
      params.tail.foldLeft(params.headOption.getOrElse(""))((a, b) => s"$a, $b")
    } else ""

    println(s"mutableState: ${mutableState}")
    println(jsre.eval("js", "a"))

    val methodString: String = s"this[$methodName]($paramString)"

    println(s"$methodName($paramString)")
    if(params.nonEmpty) {
      println(s"getFromState try: " + jsre.eval("js", s"getFromState(${params(0)}, ${params(1)})"))
    }
    println(jsre.eval("js", "a"))

    //Evaluate the method on the built script context
    jsre.eval("js", s"""$methodName($paramString)""")

    val newState: Map[String, String] = mutableState.map { s =>
      s._1 -> bindings.getMember(s._1).toString
    }

    val checkState: Json = mutableState.map{ s =>

        println(s"s._2.name: ${s._2.name}")

        s._2.name match {
          case "Number" => println("match: "+bindings.getMember(s._1).asInt()); s._1 -> JsonNumber.fromString(bindings.getMember(s._1).toString).get.asJson
          case "String" => s._1 -> bindings.getMember(s._1).asString.asJson
          case _ => throw new NoSuchElementException
        }
      }.asJson

      checkState
  }

  // TODO Fix instantiation to handle runtime input and/or extract to a better location
  val forgingSettings = new ForgingSettings {
    override def settingsJSON: Map[String, Json] = super.settingsFromFile("testSettings.json")
  }

  val sbr: StateBoxRegistry = StateBoxRegistry.readOrGenerate(forgingSettings)
  val storage: BifrostHistory = BifrostHistory.readOrGenerate(forgingSettings)

  //def getStatebox(): StateBox =
}