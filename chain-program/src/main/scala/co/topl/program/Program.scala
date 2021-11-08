package co.topl.program

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.crypto.PublicKey
import co.topl.modifier.box.{CodeBox, StateBox}
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.exceptions.{ChainProgramException, JsonParsingException}
import io.circe._
import io.circe.syntax._
import org.graalvm.polyglot.{Context, Value}
import co.topl.codecs._

import scala.util.Try

/**
 * @param parties          Public keys allowed to interact with the program
 * @param lastUpdated      timestamp of last update made to the program
 * @param id               Unique identifier
 * @param executionBuilder Context for the state and code to execute methods on the program
 */
case class Program(
  parties:          Map[PublicKeyPropositionCurve25519, String],
  lastUpdated:      Long,
  id:               Array[Byte],
  executionBuilder: Json
) {

  val MIN_PARTIES: Int = 2
  val MAX_PARTIES: Int = 1024

  if (parties.size < MIN_PARTIES || parties.size > MAX_PARTIES) {
    throw new ChainProgramException(
      "An invalid number of parties was specified for the program " +
      "(must be between 2 and 1024)."
    )
  }

  val jsre: Context = Context.create("js")

  val executionBuilderObj: ExecutionBuilder = executionBuilder.as[ExecutionBuilder] match {
    case Right(a) => a
    case Left(_)  => throw new JsonParsingException("Was unable to parse a valid executionBuilder from provided JSON")
  }

  jsre.eval("js", ProgramPreprocessor.objectAssignPolyfill)

  //noinspection ScalaStyle
  def applyFunction(methodName: String)(args: JsonObject)(params: Array[String]) /*: Try[(Program, Option[Json])]*/ =
    Try {}

  lazy val json: Json = Map(
    "executionBuilder" -> executionBuilder,
//    "parties" -> parties
//      .map(p => {
//        p._1.address -> p._2.asJson
//      })
//      .asJson,
    "lastUpdated" -> lastUpdated.asJson,
    "id"          -> id.encodeAsBase58.asJson
  ).asJson

}

object Program {

  def apply(programJson: Json, id: Array[Byte]): Program = {
    val jsonMap: Map[String, Json] = programJson.asObject
      .map(_.toMap)
      .get

    val parties: Map[PublicKeyPropositionCurve25519, String] = jsonMap("parties").asObject match {
      case Some(partiesObject) =>
        partiesObject.toMap
          .map { party =>
            val publicKey =
              Base58Data.validated(party._1).map(data => PublicKey(data.encodeAsBytes)).getOrThrow()
            val role = party._2.asString.get
            new PublicKeyPropositionCurve25519(publicKey) -> role
          }
      case None => throw new JsonParsingException(s"Error: ${jsonMap("parties")}")
    }

    new Program(
      parties, //TODO #22 new PublicKey25519Proposition(Base58.decode(jsonMap("producer").asString.get).get),
      jsonMap("lastUpdated").asNumber.get.toLong.getOrElse(0L),
      id,
      jsonMap("executionBuilder")
    )
  }

  /**
   * @param stateBoxes Set of StateBoxes to form program
   * @param codeBoxes  Set of CodeBoxes to form program
   * @param methodName The method to be called on the program
   * @param party      Public key making the call
   * @param args       parameters for the method
   * @return State members to update the mutable StateBox
   */
  //noinspection ScalaStyle
  def execute(stateBoxes: Seq[StateBox], codeBoxes: Seq[CodeBox], methodName: String)(
    party:                PublicKeyPropositionCurve25519
  )(args:                 JsonObject): Try[Json] =
    Try {
      val chainProgramInterface = createProgramInterface(codeBoxes)

      methodCheck(methodName, args, chainProgramInterface)

      val jsre: Context = Context.create("js")
      val bindings = jsre.getBindings("js")

      val mutableState: Map[String, Json] = stateBoxes.head.state.asObject.get.toMap
      val preparedState: String = mutableState
        .map { st =>
          s"${st._1} = ${st._2}"
        }
        .mkString("\n")

      val programCode: String = codeBoxes.foldLeft("")((a, b) => a ++ b.code.foldLeft("")((a, b) => a ++ (b + "\n")))

      //Pass in JSON objects for each read-only StateBox
      stateBoxes.drop(1).foreach { sb =>
        val stateBoxId: String = "_" + sb.value.toString.replace("-", "_")
        jsre.eval("js", s"""var $stateBoxId = JSON.parse(${sb.state})""")
      }

      //Inject function to read from read only StateBoxes
      val getFromState =
        s"""
           |function getFromState(id, value) {
           |  return this[id][value]
           |}
       """.stripMargin

      jsre.eval("js", getFromState)

      //Pass in writable state and functions
      jsre.eval("js", preparedState)
      jsre.eval("js", programCode)

      val params: Array[String] = args.toMap.values.toArray.map(_.noSpaces)
      val paramString: String = if (params.nonEmpty) {
        params.tail.foldLeft(params.headOption.getOrElse(""))((a, b) => s"""$a, $b""")
      } else ""

      //Evaluate the method on the built script context
      jsre.eval("js", s"""$methodName($paramString)""")

      val returnState: Map[String, Json] = mutableState.map { s =>
        val valueType: String = jsre.eval("js", s"typeof ${s._1}").asString

        bindings.getMember(s._1) match {
          case value: Value => s._1 -> stateTypeCheck(s, value, valueType)
          case _ => throw new NoSuchElementException(s"""Element "${s._2.name}" does not exist in program state""")
        }
      }

      returnState.asJson
    }

  private def methodCheck(methodName: String, args: JsonObject, interface: Map[String, Seq[String]]): Unit = {
    val params: Seq[String] = interface(methodName)

    args.toMap.zip(params).map { p =>
      p._1._2.name match {
        case p._2 =>
        case _    => throw new Exception("Argument types do not match chain program method parameter types")
      }
    }
  }

  private def stateTypeCheck(variable: (String, Json), member: Value, memberType: String): Json =
    if (variable._2.name == memberType.capitalize)
      variable._2.name match {
        //TODO Check for all valid JS types
        case "Number" => JsonNumber.fromString(member.toString).get.asJson
        case "String" => member.as(classOf[String]).asJson
        case _ => throw new NoSuchElementException(s"""Element "${variable._1}" does not exist in program state """)
      }
    else
      throw new ClassCastException(
        s"""Updated state variable $member with type $memberType does not match original variable type of ${variable._2.name}"""
      )

  private def createProgramInterface(codeBoxes: Seq[CodeBox]): Map[String, Seq[String]] =
    codeBoxes.foldLeft(Map[String, Seq[String]]())((a, b) => a ++ b.interface)
}
