package co.topl.nodeView.state.box

import co.topl.crypto.{ PrivateKey25519, ProofOfKnowledgeProposition }
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.utils.serialization.BifrostSerializer
import io.circe.{ Decoder, Encoder, HCursor, Json }

/**
 * Created by Matthew on 4/11/2017.
 */
abstract class Box ( val proposition: ProofOfKnowledgeProposition[PrivateKey25519],
                     val nonce: Long,
                     val value: Any
                   ) extends GenericBox[ProofOfKnowledgeProposition[PrivateKey25519], Any] {

  self =>

  override type M = Box

  val typeOfBox: String

  lazy val publicKey: ProofOfKnowledgeProposition[PrivateKey25519] = proposition

  override lazy val json: Json = Box.jsonEncoder(self)

  override def serializer: BifrostSerializer[Box] = BoxSerializer

  override def equals ( obj: Any ): Boolean = obj match {
    case acc: Box => (acc.id == this.id) && acc.value == this.value
    case _        => false
  }

  override def hashCode ( ): Int = proposition.hashCode()
}


object Box {
  implicit val jsonEncoder: Encoder[Box] = {
    case box: ArbitBox     => ArbitBox.jsonEncoder(box)
    case box: PolyBox      => PolyBox.jsonEncoder(box)
    case box: AssetBox     => AssetBox.jsonEncoder(box)
    case box: ExecutionBox => ExecutionBox.jsonEncoder(box)
    case box: StateBox     => StateBox.jsonEncoder(box)
    case box: CodeBox      => CodeBox.jsonEncoder(box)
  }

  implicit val jsonDecoder: Decoder[Box] = { c: HCursor =>
    c.downField("typeOfBox").as[String].map {
      case "ArbitBox"     => ArbitBox.jsonDecoder(c)
      case "PolyBox"      => PolyBox.jsonDecoder(c)
      case "AssetBox"     => AssetBox.jsonDecoder(c)
      case "ExecutionBox" => ExecutionBox.jsonDecoder(c)
      case "StateBox"     => StateBox.jsonDecoder(c)
      case "CodeBox"      => CodeBox.jsonDecoder(c)
    } match {
      case Right(box) => box
      case Left(ex)   => throw ex
    }
  }
}
