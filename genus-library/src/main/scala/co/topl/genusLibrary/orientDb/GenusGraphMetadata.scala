package co.topl.genusLibrary.orientDb

import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Length, Sized}
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import com.tinkerpop.blueprints.impls.orient.{OrientEdgeType, OrientGraphNoTx, OrientVertexType}

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
class GenusGraphMetadata(val graphNoTx: OrientGraphNoTx) {
  import GenusGraphMetadata._

  val addressVertexType: OrientVertexType = ensureVertexSchemaInitialized(addressVertexSchema)
  val boxStateVertexType: OrientVertexType = ensureVertexSchemaInitialized(boxStateSchema)

  val blockHeaderVertexType: OrientVertexType = ensureVertexSchemaInitialized(blockHeaderSchema)

  val blockBodyVertexType: OrientVertexType = graphNoTx.createVertexType("BlockBody")
  val boxVertexType: OrientVertexType = graphNoTx.createVertexType("Box")
  val transactionVertexType: OrientVertexType = graphNoTx.createVertexType("Transaction")

  val currentBoxStateEdgeType: OrientEdgeType = graphNoTx.createEdgeType("CurrentBoxState")
  val prevToNextBoxStateEdgeType: OrientEdgeType = graphNoTx.createEdgeType("PrevToNext")
  val stateToBoxEdgeType: OrientEdgeType = graphNoTx.createEdgeType("StateToBox")
  val inputEdgeType: OrientEdgeType = graphNoTx.createEdgeType("Input")
  val outputEdgeType: OrientEdgeType = graphNoTx.createEdgeType("Output")

  def ensureVertexSchemaInitialized(vertexSchema: VertexSchema[_]): OrientVertexType =
    Option(graphNoTx.getVertexType(vertexSchema.name)).getOrElse {
      val vertexType = graphNoTx.createVertexType(vertexSchema.name)
      vertexSchema.properties.foreach(property =>
        property.propertyAttributeSetter({
          val vertexProperty = vertexType.createProperty(property.name, property.propertyType)
          vertexProperty.setReadonly(true).setMandatory(true)
          vertexProperty
        })
      )
      vertexSchema.indices.foreach(index =>
        vertexType.createIndex(index.name, index.indexType, index.propertyNames: _*)
      )
      vertexType
    }
}

object GenusGraphMetadata {

  /**
   * Regular expression for the base58 representation of typed evidence.
   */
  private val TypedEvidenceRegex = "^[123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz]{33,46}$"

  import OrientDbTyped.Instances._

  // TODO: Rework to use data model classes generated from protobuf definitions rather than those in the models project.

  /////////////////////////////////////////////////////////////////////////////////
  //                                                                             //
  // The following Vals are schemas for nodes and edges.                         //
  //                                                                             //
  /////////////////////////////////////////////////////////////////////////////////

  /**
   * Schema for Address nodes
   */
  private val addressVertexSchema: VertexSchema[TypedEvidence] =
    VertexSchema.create(
      "Address",
      GraphDataEncoder[TypedEvidence]
        .withProperty(
          "typePrefix",
          p => java.lang.Byte.valueOf(p.typePrefix),
          _.setMandatory(true).setReadonly(true).setNotNull(true)
        )
        .withProperty("evidence", _.evidence.data.toArray, _.setMandatory(true).setReadonly(true).setNotNull(true))(
          byteArrayOrientDbTypes
        )
        .withIndex("addressIndex", INDEX_TYPE.UNIQUE, "typePrefix", "evidence"),
      v => TypedEvidence(v("typePrefix"), v("evidence"))
    )

  /**
   * Schema for TxO state vertexes
   * <p>
   *   Txo state vertexes have no properties, just links to boxes.
   */
  private val boxStateSchema: VertexSchema[Unit] =
    VertexSchema.create(
      "boxState",
      GraphDataEncoder[Unit],
      v => ()
    )

  private val blockHeaderSchema: VertexSchema[BlockHeaderV2] =
    VertexSchema.create(
      "BlockHeader",
      GraphDataEncoder[BlockHeaderV2]
        .withProperty(
          "blockId",
          b => {
            val (typePrefix, bytes) = TetraIdentifiableInstances.identifiableBlockHeaderV2.idOf(b)
            typedBytesTupleToByteArray((typePrefix, bytes.toArray))
          },
          _.setNotNull(true)
        )(byteArrayOrientDbTypes)
        .withProperty("parentHeaderId", p => typedBytesToByteArray(p.parentHeaderId), _.setNotNull(true))(
          byteArrayOrientDbTypes
        )
        .withProperty("parentSlot", l => java.lang.Long.valueOf(l.parentSlot), _.setMandatory(false))(longOrientDbTyped)
        .withProperty("txRoot", _.txRoot.data.toArray, _.setMandatory(false))(byteArrayOrientDbTypes)
        .withProperty("bloomFilter", _.bloomFilter.data.toArray, _.setMandatory(false))(byteArrayOrientDbTypes)
        .withProperty("timestamp", ts => java.lang.Long.valueOf(ts.timestamp), _.setNotNull(true))(longOrientDbTyped)
        .withProperty("height", ht => java.lang.Long.valueOf(ht.height), _.setNotNull(true))(longOrientDbTyped)
        .withProperty("slot", s => java.lang.Long.valueOf(s.slot), _.setNotNull(true))(longOrientDbTyped)
        .withProperty(
          "eligibilityCertificate",
          e => eligibilityCertificateToByteArray(e.eligibilityCertificate),
          _.setNotNull(true)
        )(byteArrayOrientDbTypes)
        .withProperty(
          "operationalCertificate",
          o => operationalCertificateToByteArray(o.operationalCertificate),
          _.setNotNull(true)
        )(byteArrayOrientDbTypes)
        .withProperty("metadata", _.metadata.map(_.data.bytes).orNull, _.setNotNull(false))(byteArrayOrientDbTypes)
        .withProperty("address", s => stakingAddressOperatorToByteArray(s.address), _.setNotNull(true))(
          byteArrayOrientDbTypes
        )
        .withIndex("blockHeaderIndex", INDEX_TYPE.UNIQUE, "blockId"),
      v =>
        BlockHeaderV2(
          byteArrayToTypedBytes(v("parentHeaderId")),
          v("parentSlot"),
          v("txRoot"),
          v("bloomFilter"),
          v("timestamp"),
          v("height"),
          v("slot"),
          v("eligibilityCertificate"),
          v("operationalCertificate"),
          v("metadata"),
          v("address")
        )
    )

  // TODO this needs to change when we switch to models from protobufs

  def typedBytesToByteArray(t: TypedIdentifier): Array[Byte] =
    typedBytesTupleToByteArray((t.typePrefix, t.dataBytes.toArray))

  def byteArrayToTypedBytes(a: Array[Byte]): TypedBytes = {
    val tuple = byteArrayToTypedBytesTuple(a)
    TypedBytes(tuple._1, Bytes(tuple._2))
  }

  def byteArrayToTypedBytesTuple(a: Array[Byte]): (TypePrefix, Array[TypePrefix]) = (a(0), a.drop(1))

  def typedBytesTupleToByteArray(id: (TypePrefix, Array[Byte])): Array[Byte] = {
    val a: Array[Byte] = new Array(1 + id._2.length)
    a(0) = id._1
    Array.copy(id._2, 0, a, 1, id._2.length)
    a
  }

  private val evidenceLength: Length = implicitly[Evidence.Length]
  private val etaLength = implicitly[Eta.Length].value
  private val vrfSigLength = implicitly[Proofs.Knowledge.VrfEd25519.Length].value
  private val vkVRFLength = implicitly[VerificationKeys.VrfEd25519.Length].value

  // No need for a byteArrayToBlockHeaderId because it is computed rather than stored.
  def eligibilityCertificateToByteArray(eligibilityCertificate: EligibilityCertificate): Array[Byte] = {
    val serializedLength = vrfSigLength + vkVRFLength + evidenceLength.value + etaLength
    val a = new Array[Byte](serializedLength)
    Array.copy(eligibilityCertificate.vrfSig, 0, a, 0, vrfSigLength)
    Array.copy(eligibilityCertificate.vkVRF, 0, a, vrfSigLength, vkVRFLength)
    Array.copy(
      eligibilityCertificate.thresholdEvidence.data.toArray,
      0,
      a,
      vrfSigLength + vkVRFLength,
      evidenceLength.value
    )
    Array.copy(
      eligibilityCertificate.eta.data.toArray,
      0,
      a,
      vrfSigLength + vkVRFLength + evidenceLength.value,
      etaLength
    )
    a
  }

  def byteArrayToEligibilityCertificate(a: Array[Byte]): EligibilityCertificate = {
    val vrfSigArray = copyArraySlice(a, 0, vrfSigLength)
    val vrfSig = Proofs.Knowledge.VrfEd25519(Sized.strictUnsafe(Bytes(vrfSigArray)))

    val vkVRFArray = copyArraySlice(a, vrfSigLength, vkVRFLength)
    val vkVRF = VerificationKeys.VrfEd25519(Sized.strictUnsafe(Bytes(vkVRFArray)))

    val evidenceArray = copyArraySlice(a, vrfSigLength + vkVRFLength, evidenceLength.value)
    val thresholdEvidence: Evidence = Sized.strictUnsafe(Bytes(evidenceArray))

    val etaArray = copyArraySlice(a, vrfSigLength + vkVRFLength + evidenceLength.value, etaLength)
    val eta: Eta = Sized.strictUnsafe(Bytes(etaArray))

    EligibilityCertificate(vrfSig, vkVRF, thresholdEvidence, eta)
  }

  def copyArraySlice(fromArray: Array[Byte], offset: Int, length: Int): Array[Byte] = {
    val toArray = new Array[Byte](length)
    Array.copy(fromArray, offset, toArray, 0, length)
    toArray
  }

  private val parentVKLength = implicitly[VerificationKeys.KesProduct.Length].value
  private val parentSignatureLength = implicitly[Proofs.Knowledge.KesProduct.DigestLength].value
  private val childVKLength = implicitly[VerificationKeys.Ed25519.Length].value
  private val childSignatureLength = implicitly[Proofs.Knowledge.Ed25519.Length].value

  def operationalCertificateToByteArray(operationalCertificate: OperationalCertificate): Array[Byte] = {
    val serializedLength = parentVKLength + parentSignatureLength + childVKLength + childSignatureLength
    val a = new Array[Byte](serializedLength)
    Array.copy(operationalCertificate.parentVK, 0, a, 0, parentVKLength)
    Array.copy(operationalCertificate.parentSignature, 0, a, parentVKLength, parentSignatureLength)
    Array.copy(operationalCertificate.childVK, 0, a, parentVKLength + parentSignatureLength, childVKLength)
    Array.copy(
      operationalCertificate.childSignature,
      0,
      a,
      parentVKLength + parentSignatureLength + childVKLength,
      childSignatureLength
    )
    a
  }

  def byteArrayToOperationalCertificate(a: Array[Byte]): OperationalCertificate = ???

  def stakingAddressOperatorToByteArray(eligibilityCertificate: StakingAddresses.Operator): Array[Byte] = ???

  def byteArrayToStakingAddressOperator(a: Array[Byte]): StakingAddresses.Operator = ???
}
