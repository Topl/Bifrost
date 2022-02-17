package co.topl.codecs

/**
 * Contains Scodec implicit typeclasses and implicit instances as well as
 * helper typeclasses `Persistable`, `Transmittable`, and `BinaryShow` and their respective instances.
 *
 * @example {{{
 * import co.topl.codecs.binary._
 *
 * val modifierId: ModifierId = ...
 *
 * // get byte representation
 * modifierId.encodeAsBytes
 *
 * // get base 58 representation
 * modifier.encodeAsBase58
 *
 * // get hex representation
 * modifierId.encodeAsBase16
 * }}}
 */
package object binary extends BinaryCodecs
