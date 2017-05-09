package examples.bifrost.contract

import java.time.Instant

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}


class Contract(val Producer: PublicKey25519Proposition,
               val Hub: PublicKey25519Proposition,
               val Investor: PublicKey25519Proposition,
               val storage: JsonObject,
               val agreement: JsonObject,
               val id: Array[Byte]) {

  lazy val json: Json = Map(
    "producer" -> Base58.encode(Producer.pubKeyBytes).asJson,
    "hub" -> Base58.encode(Hub.pubKeyBytes).asJson,
    "investor" -> Base58.encode(Investor.pubKeyBytes).asJson,
    "agreement" -> agreement.asJson,
    "storage" -> storage.asJson
  ).asJson

  def complete(actor: PublicKey25519Proposition): Try[Contract] = {
    if (Producer.pubKeyBytes sameElements actor.pubKeyBytes) {

    } else if (Hub.pubKeyBytes sameElements actor.pubKeyBytes) {

    } else if (Producer.pubKeyBytes sameElements actor.pubKeyBytes) {

    } else {

    }

    Success(this)
  }

  def currentStatus(): Try[Json] = Try {
    this.storage("status").get
  }

  def deliver(producer: PublicKey25519Proposition): Try[Contract] = {
    if (producer.pubKeyBytes sameElements Producer.pubKeyBytes) {

    }

    Success(this)
  }

  def confirmDelivery(hub: PublicKey25519Proposition): Try[Contract] = {
    if (hub.pubKeyBytes sameElements Hub.pubKeyBytes) {

    }

    Success(this)
  }

  def checkExpiration(): Try[Json] = {
    val expiration: Long = storage("agreement").get.asObject.get.apply("expirationTimestamp").get.asNumber.get.toLong.get
    Success((Instant.now().toEpochMilli > expiration).asJson)
  }

}

object Contract {

  // get runtime mirror
  val rm = ru.runtimeMirror(getClass.getClassLoader)

  val contractMethods: Map[String, ru.MethodSymbol] = ru.typeOf[Contract].decls.map(d => d.asMethod.name.toString -> d.asMethod).toMap

  def apply(cs: Json, id: Array[Byte]): Contract = {
    val jsonMap = cs.asObject.get.toMap

    new Contract(
      new PublicKey25519Proposition(Base58.decode(jsonMap("producer").asString.get).get),
      new PublicKey25519Proposition(Base58.decode(jsonMap("hub").asString.get).get),
      new PublicKey25519Proposition(Base58.decode(jsonMap("investor").asString.get).get),
      jsonMap("storage").asObject.get,
      jsonMap("agreement").asObject.get,
      id
    )
  }

  def execute(c: Contract, methodName: String)(args: JsonObject): Try[Either[Contract, Json]] = Try {

    (contractMethods.get(methodName) match {
      case Some(m: ru.MethodSymbol) => rm.reflect(c).reflectMethod(m)(
        m.paramLists.head.map(p => {
          p.typeSignature.typeSymbol.asClass.name match {
            case "PublicKey25519Proposition" => args(p.name.toString).get.as[PublicKey25519Proposition].right.get
            case _ => decode(args(p.name.toString).get.noSpaces)
          }
        }):_*
      )
      case _ => throw new MatchError(s"Could not find method <$methodName>")
    }) match {
      case c: Success[Contract] => Left(c.value)
      case j: Success[Json] => Right(j.value)
      case f: Failure[Any] => throw f.exception
    }
  }

}