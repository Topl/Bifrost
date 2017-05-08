package examples.bifrost.contract

import java.lang.reflect.Method
import java.time.Instant

import io.circe.syntax._
import io.circe.{Json, JsonObject}
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

  val complete: (PublicKey25519Proposition) => Try[Contract] = (actor: PublicKey25519Proposition) => {
    if (Producer.pubKeyBytes sameElements actor.pubKeyBytes) {

    } else if (Hub.pubKeyBytes sameElements actor.pubKeyBytes) {

    } else if (Producer.pubKeyBytes sameElements actor.pubKeyBytes) {

    } else {

    }

    Success(this)
  }

  val currentStatus: () => Try[Json] = () => Try {
    this.storage("status").get
  }

  val deliver: (PublicKey25519Proposition) => Try[Contract] = (producer: PublicKey25519Proposition) => {
    if (producer.pubKeyBytes sameElements Producer.pubKeyBytes) {

    }

    Success(this)
  }

  val confirmDelivery: (PublicKey25519Proposition) => Try[Contract] = (hub: PublicKey25519Proposition) => {
    if (hub.pubKeyBytes sameElements Hub.pubKeyBytes) {

    }

    Success(this)
  }

  val checkExpiration: () => Try[Json] = () => {
    val expiration: Long = storage("agreement").get.asObject.get.apply("expirationTimestamp").get.asNumber.get.toLong.get
    Success((Instant.now().toEpochMilli > expiration).asJson)
  }

}

object Contract {

  val contractMethods: Map[String, Method] = classOf[Contract].getMethods.map(m => m.getName -> m)(collection.breakOut)

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

  def execute(c: Contract, methodName: String)(args: Seq[AnyRef]) : Try[Either[Contract, Json]] = Try {

    (contractMethods.get(methodName) match {
      case Some(m: Method) => m.invoke(c, args:_*)
      case _ => throw new MatchError(s"Could not find method <$methodName>")
    }) match {
      case c: Success[Contract] => Left(c.value)
      case j: Success[Json] => Right(j.value)
      case f: Failure[Any] => throw f.exception
    }
  }

}