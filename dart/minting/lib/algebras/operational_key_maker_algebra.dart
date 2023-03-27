import 'package:bifrost_crypto/ed25519.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';

abstract class OperationalKeyMakerAlgebra {
  Future<OperationalKeyOut?> operationalKeyForSlot(
      Int64 slot, SlotId parentSlotId);
}

class OperationalKeyOut {
  final Int64 slot;
  final Ed25519KeyPair childKeyPair;
  final SignatureKesProduct parentSignature;
  final VerificationKeyKesProduct parentVK;

  OperationalKeyOut(
      this.slot, this.childKeyPair, this.parentSignature, this.parentVK);
}
