package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.{SeriesId, TransactionOutputAddress}
import co.topl.brambl.syntax.groupPolicyAsGroupPolicySyntaxOps
import co.topl.genusLibrary.orientDb.schema.OIndexable.Instances.groupPolicy
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}
import com.google.protobuf.ByteString

object SchemaGroupPolicy {

  /**
   * SchemaGroupPolicy model:
   *
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/brambl/models/event.proto#L61
   */
  object Field {
    val SchemaName = "GroupPolicy"
    val Label = "label"
    val RegistrationUtxo = "registrationUtxo"
    val FixedSeries = "fixedSeries"
    val GroupPolicyId = "groupPolicyId"
    val GroupPolicyIndex = "groupPolicyIndex"
  }

  def make(): VertexSchema[GroupPolicy] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[GroupPolicy]
        // @formatter:off
        .withProperty(Field.GroupPolicyId, _.computeId.value.toByteArray, mandatory = true, readOnly = true, notNull= true)
        .withProperty(Field.Label, _.label, mandatory = true,readOnly = true,  notNull = true )
        .withProperty(Field.RegistrationUtxo, _.registrationUtxo.toByteArray,  mandatory = true, readOnly = true, notNull = true )
        .withProperty(Field.FixedSeries, _.fixedSeries.map(_.value.toByteArray).getOrElse(Array.empty[Byte]), mandatory = false, readOnly = true, notNull = false)
        .withIndex[GroupPolicy](Field.GroupPolicyIndex, Field.GroupPolicyId),
      // @formatter:on
      v =>
        GroupPolicy(
          label = v(Field.Label): String,
          registrationUtxo = TransactionOutputAddress.parseFrom(v(Field.RegistrationUtxo): Array[Byte]),
          fixedSeries = {
            val bytes = v(Field.FixedSeries): Array[Byte]
            Option.when(bytes.nonEmpty)(SeriesId(value = ByteString.copyFrom(bytes)))
          }
        )
    )

}
