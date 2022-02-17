//package co.topl.codecs.binary.legacy.modifier.box
//
//import co.topl.attestation.Evidence
//import co.topl.codecs.binary.legacy.attestation.EvidenceSerializer
//import co.topl.codecs.binary.legacy.{Reader, Writer}
//import co.topl.modifier.box.{ProgramBox, ProgramId}
//
//object ProgramBoxSerializer {
//
//  def serialize(obj: ProgramBox, w: Writer): Unit = {
//    EvidenceSerializer.serialize(obj.evidence, w)
//
//    w.putLong(obj.nonce)
//
//    // box identifier in the program box registry
//    ProgramIdSerializer.serialize(obj.value, w)
//  }
//
//  def parse(r: Reader): (Evidence, Long, ProgramId) =
//    (EvidenceSerializer.parse(r), r.getLong(), ProgramIdSerializer.parse(r))
//}
