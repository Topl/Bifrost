import 'package:bifrost_crypto/ed25519.dart' show ed25519;
import 'package:bifrost_minting/algebras/operational_key_maker_algebra.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/quivr/models/shared.pb.dart';

class OperationalKeyMaker extends OperationalKeyMakerAlgebra {
  @override
  Future<OperationalKeyOut?> operationalKeyForSlot(
      Int64 slot, SlotId parentSlotId) async {
    final cryptoKeyPair = await ed25519.generateKeyPair();
    return OperationalKeyOut(
      slot,
      KeyPair(
        sk: SigningKey(value: cryptoKeyPair.sk),
        vk: VerificationKey(value: cryptoKeyPair.vk),
      ),
      SignatureKesProduct(),
      VerificationKeyKesProduct(),
    );
  }
}
