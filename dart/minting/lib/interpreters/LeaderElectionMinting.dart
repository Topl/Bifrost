import 'package:bifrost_common/models/ratio.dart';
import 'package:bifrost_minting/algebras/LeaderElectionMintingAlgebra.dart';
import 'package:bifrost_minting/algebras/vrf_calculator_algebra.dart';
import 'package:bifrost_minting/models/vrf_hit.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/eligibility_certificate.pb.dart';

class LeaderElectionMinting extends LeaderElectionMintingAlgebra {
  final List<int> _vrfVK;
  // TODO: LeaderElectionValidation algebra
  final VrfCalculatorAlgebra _vrfProofs;

  LeaderElectionMinting(this._vrfVK, this._vrfProofs);

  @override
  Future<VrfHit?> getHit(
      Ratio relativeStake, Int64 slot, Int64 slotDiff, List<int> eta) async {
    final Ratio threshold = Ratio(); // TODO
    final testProof = await _vrfProofs.proofForSlot(slot, eta);
    final rho = await _vrfProofs.rhoForSlot(slot, eta);
    final isLeader = 2 > 1; // TODO
    if (isLeader) {
      return VrfHit(
        EligibilityCertificate(vrfSig: testProof, vrfVK: _vrfVK, eta: eta),
        slot,
        threshold,
      );
    }
    ;
  }
}
