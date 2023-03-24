import 'package:bifrost_crypto/Ed25519.dart';
import 'package:bifrost_minting/algebras/operational_key_maker_algebra.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';

class OperationalKeys extends OperationalKeyMakerAlgebra {
  @override
  Future<OperationalKeyOut?> operationalKeyForSlot(
      Int64 slot, SlotId parentSlotId) async {
    // TODO
    return OperationalKeyOut(
      slot,
      await Ed25519.generateKeyPair(),
      SignatureKesProduct(),
      VerificationKeyKesProduct(),
    );
  }
}
