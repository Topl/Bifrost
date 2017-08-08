package bifrost.contract

import java.time.Instant

import bifrost.contract.modules.BaseModuleWrapper
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import jdk.nashorn.api.scripting.{NashornScriptEngine, NashornScriptEngineFactory}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.collection.{SortedSet, mutable}
import scala.util.{Failure, Success, Try}


case class Contract(Producer: PublicKey25519Proposition,
                    Hub: PublicKey25519Proposition,
                    Investor: PublicKey25519Proposition,
                    lastUpdated: Long,
                    id: Array[Byte],
                    agreement: Json) {

  lazy val jsre: NashornScriptEngine = new NashornScriptEngineFactory().getScriptEngine.asInstanceOf[NashornScriptEngine]

  def applyFunction(methodName: String)(params: JsonObject): Try[(Contract, Option[Json])] = Try {

    jsre.eval((agreement \\ "initjs").head.noSpaces)
    jsre.eval(s"var c = ${(agreement \\ "name").head.noSpaces}.fromJSON(${(agreement \\ "state").head.noSpaces})")

    val update = s"""
      |var res = c[$methodName](${params.asJson.noSpaces});
      |if(res instanceof ${(agreement \\ "name").head.noSpaces}) {
      |  return {
      |    "__returnedUpdate": true,
      |    "contract": res.toJSON()
      |  }
      |} else {
      |  return {
      |    "__returnedUpdate": false
      |    "contract": c.toJSON(),
      |    "functionResult": res
      |  }
      |}
    """.stripMargin

    val result = parse(jsre.eval(update).asInstanceOf[String]).right.get

    val resultingContract = this.copy(
      agreement = agreement.mapObject(
        _.add("state", (result \\ "contract").head)
      ),
      lastUpdated = Instant.now.toEpochMilli
    )

    val functionResult: Option[Json] = if((result \\ "__returnedUpdate").head.asBoolean.get) {
      Some((result \\ "functionResult").head)
    } else {
      None
    }

    (resultingContract, functionResult)
  }

  def getFromContract(property: String): Try[Json] = Try {
    val core = (agreement \\ "core").head.as[BaseModuleWrapper].right.get
    jsre.eval(core.initjs)
    jsre.eval(s"var c = ${core.name}.fromJSON(${core.state.noSpaces})")

    val update = s"c.$property"

    println(jsre.eval(update))

    parse(jsre.eval(update).asInstanceOf[String]).right.get
  }

  lazy val json: Json = Map(
    "agreement" -> agreement,
    "producer" -> Base58.encode(Producer.pubKeyBytes).asJson,
    "investor" -> Base58.encode(Investor.pubKeyBytes).asJson,
    "hub" -> Base58.encode(Hub.pubKeyBytes).asJson,
    "lastUpdated" -> lastUpdated.asJson,
    "id" -> Base58.encode(id).asJson
  ).asJson

}

object Contract {

  def apply(cs: Json, id: Array[Byte]): Contract = {
    val jsonMap = cs.asObject.get.toMap

    new Contract(
      new PublicKey25519Proposition(Base58.decode(jsonMap("producer").asString.get).get),
      new PublicKey25519Proposition(Base58.decode(jsonMap("hub").asString.get).get),
      new PublicKey25519Proposition(Base58.decode(jsonMap("investor").asString.get).get),
      jsonMap("lastUpdated").asNumber.get.toLong.getOrElse(0L),
      id,
      jsonMap("agreement")
    )
  }

  def execute(c: Contract, methodName: String)(party: PublicKey25519Proposition)(args: JsonObject): Try[Either[Contract, Json]] = Try {

    val methodAttempt: Option[mutable.LinkedHashSet[String]] = ((c.agreement \\ "registry").head \\ methodName).headOption.map(_.as[mutable.LinkedHashSet[String]].toOption).map(_.get)

    methodAttempt match {
      case Some(params: mutable.LinkedHashSet[String]) =>

        val res = c.applyFunction(methodName)(args.filterKeys(k => params.contains(k)))

        res match {
          case Success((c: Contract, None)) => Left(c)
          case Success((_, Some(j: Json))) => Right(j)
          case f: Failure[_] => throw f.exception
        }

      case _ => throw new MatchError(s"Could not find method <$methodName>")
    }
  }

  object Status extends Enumeration {
    type Status = Value
    val EXPIRED: Status = Value("expired")
    val COMPLETE: Status = Value("complete")
    val INITIALISED: Status = Value("initialised")
    val UNKNOWN: Status = Value("unknown")

    implicit val decodeStatus: Decoder[Status.Value] = Decoder.enumDecoder(Status)
    implicit val encodeStatus: Encoder[Status.Value] = Encoder.enumEncoder(Status)

  }

}