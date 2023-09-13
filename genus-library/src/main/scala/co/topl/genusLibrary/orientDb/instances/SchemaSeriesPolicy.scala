package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.Event.SeriesPolicy
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.{FungibilityType, QuantityDescriptorType}
import co.topl.brambl.syntax.seriesPolicyAsSeriesPolicySyntaxOps
import co.topl.genusLibrary.orientDb.schema.OIndexable.Instances.seriesPolicy
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}
import com.google.protobuf.struct.Struct

object SchemaSeriesPolicy {

  /**
   * Series Policy model:
   *
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/brambl/models/event.proto#L70
   */
  object Field {
    val SchemaName = "SeriesPolicy"
    val Label = "label"
    val TokenSupply = "tokenSupply"
    val RegistrationUtxo = "registrationUtxo"
    val QuantityDescriptor = "quantityDescriptor"
    val Fungibility = "fungibility"
    val EphemeralMetadataScheme = "ephemeralMetadataScheme"
    val PermanentMetadataScheme = "permanentMetadataScheme"

    val SeriesPolicyId = "seriesPolicyId"
    val SeriesPolicyIndex = "seriesPolicyIndex"
  }

  def make(): VertexSchema[SeriesPolicy] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[SeriesPolicy]
        // @formatter:off
        .withProperty(Field.SeriesPolicyId, _.computeId.value.toByteArray, mandatory = true, readOnly = true, notNull= true)
        .withProperty(Field.Label, _.label, mandatory = true,readOnly = true,  notNull = true )
        .withProperty(Field.TokenSupply, seriesPolicy => java.lang.Integer.valueOf(seriesPolicy.tokenSupply.getOrElse(0)), mandatory = true, readOnly = true, notNull= true)
        .withProperty(Field.RegistrationUtxo, _.registrationUtxo.toByteArray,  mandatory = true, readOnly = true, notNull = true )
        .withProperty(Field.QuantityDescriptor, seriesPolicy => java.lang.Integer.valueOf(seriesPolicy.quantityDescriptor.value), mandatory = true, readOnly = true, notNull= true)
        .withProperty(Field.Fungibility, seriesPolicy => java.lang.Integer.valueOf(seriesPolicy.fungibility.value), mandatory = true, readOnly = true, notNull= true)
        .withProperty(Field.EphemeralMetadataScheme, _.ephemeralMetadataScheme.map(_.toByteArray).getOrElse(Array.empty[Byte]), mandatory = true, readOnly = true, notNull= true)
        .withProperty(Field.PermanentMetadataScheme, _.permanentMetadataScheme.map(_.toByteArray).getOrElse(Array.empty[Byte]), mandatory = true, readOnly = true, notNull= true)
        .withIndex[SeriesPolicy](Field.SeriesPolicyIndex, Field.SeriesPolicyId),
      // @formatter:on
      v =>
        SeriesPolicy(
          label = v(Field.Label): String,
          tokenSupply = {
            val i = v(Field.TokenSupply): Int
            Option.when(i != 0)(i)
          },
          registrationUtxo = TransactionOutputAddress.parseFrom(v(Field.RegistrationUtxo): Array[Byte]),
          quantityDescriptor = QuantityDescriptorType.fromValue(v(Field.QuantityDescriptor): Int),
          fungibility = FungibilityType.fromValue(v(Field.Fungibility): Int),
          ephemeralMetadataScheme = {
            val bytes = v(Field.EphemeralMetadataScheme): Array[Byte]
            Option.when(bytes.nonEmpty)(Struct.parseFrom(bytes))
          },
          permanentMetadataScheme = {
            val bytes = v(Field.PermanentMetadataScheme): Array[Byte]
            Option.when(bytes.nonEmpty)(Struct.parseFrom(bytes))
          }
        )
    )

}
