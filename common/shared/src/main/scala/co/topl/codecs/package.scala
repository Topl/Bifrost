package co.topl

/**
 * This package contains typeclass instances for JSON and Binary data type encodings of all types in the `common` package.
 *
 * The sub-packages `json` and `binary` contain Circe codecs and Scodec codecs respectively.
 *
 * @example {{{
 * // imports all typeclasses and implicit instances
 * import co.topl.codecs._
 *
 * // imports all Circe json `Encoder` and `Decoder` implicit instances
 * import co.topl.codecs.json._
 *
 * // imports all Scodec binary `Codec` implicit instances with additional helper typeclasses
 * // and instances
 * import co.topl.codecs.binary._
 * }}}
 */
package object codecs extends Codecs
