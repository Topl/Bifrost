import 'package:bifrost_common/models/unsigned.dart';
import 'package:bifrost_consensus/algebras/consensus_validation_state_algebra.dart';
import 'package:bifrost_consensus/algebras/eta_calculation_algebra.dart';
import 'package:bifrost_consensus/algebras/leader_election_validation_algebra.dart';
import 'package:bifrost_consensus/utils.dart';
import 'package:bifrost_crypto/Ed25519.dart' as cryptoEd25519;
import 'package:bifrost_minting/algebras/operational_key_maker_algebra.dart';
import 'package:bifrost_minting/algebras/staking_algebra.dart';
import 'package:bifrost_minting/algebras/vrf_calculator_algebra.dart';
import 'package:bifrost_minting/models/vrf_hit.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/eligibility_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';

class Staking extends StakingAlgebra {
  final StakingAddress _address;
  final List<int> vkVrf;
  final OperationalKeyMakerAlgebra operationalKeyMaker;
  final ConsensusValidationStateAlgebra consensusValidationState;
  final EtaCalculationAlgebra etaCalculation;
  final VrfCalculatorAlgebra vrfCalculator;
  final LeaderElectionValidationAlgebra leaderElectionValidation;

  Staking(
      this._address,
      this.vkVrf,
      this.operationalKeyMaker,
      this.consensusValidationState,
      this.etaCalculation,
      this.vrfCalculator,
      this.leaderElectionValidation);

  @override
  StakingAddress get address => _address;

  @override
  Future<BlockHeader?> certifyBlock(
      SlotId parentSlotId,
      Int64 slot,
      UnsignedBlockHeader Function(PartialOperationalCertificate)
          unsignedBlockBuilder) async {
    final operationalKeyOutOpt =
        await operationalKeyMaker.operationalKeyForSlot(slot, parentSlotId);
    if (operationalKeyOutOpt != null) {
      final partialCertificate = PartialOperationalCertificate(
        operationalKeyOutOpt.parentVK,
        operationalKeyOutOpt.parentSignature,
        operationalKeyOutOpt.childKeyPair.vk.value,
      );
      final unsignedHeader = unsignedBlockBuilder(partialCertificate);
      final List<int> messageToSign = []; // TODO
      final cryptoKeyPair = cryptoEd25519.KeyPair(
          operationalKeyOutOpt.childKeyPair.sk.value,
          operationalKeyOutOpt.childKeyPair.vk.value);
      final operationalCertificate = OperationalCertificate(
        parentVK: operationalKeyOutOpt.parentVK,
        parentSignature: operationalKeyOutOpt.parentSignature,
        childVK: operationalKeyOutOpt.childKeyPair.vk.value,
        childSignature: await cryptoEd25519.Ed25519.sign(
          messageToSign,
          cryptoKeyPair,
        ),
      );
      final header = BlockHeader(
        parentHeaderId: unsignedHeader.parentHeaderId,
        parentSlot: unsignedHeader.parentSlot,
        txRoot: unsignedHeader.txRoot,
        bloomFilter: unsignedHeader.bloomFilter,
        timestamp: unsignedHeader.timestamp,
        height: unsignedHeader.height,
        slot: unsignedHeader.slot,
        eligibilityCertificate: unsignedHeader.eligibilityCertificate,
        operationalCertificate: operationalCertificate,
        metadata: unsignedHeader.metadata,
        address: unsignedHeader.address,
      );
      return header;
    }
    return null;
  }

  @override
  Future<VrfHit?> elect(SlotId parentSlotId, Int64 slot) async {
    final eta = await etaCalculation.etaToBe(parentSlotId, slot);
    final relativeStake = await consensusValidationState.operatorRelativeStake(
        parentSlotId.blockId, slot, address);
    if (relativeStake == null) return null;
    final threshold = await leaderElectionValidation.getThreshold(
        relativeStake, slot - parentSlotId.slot);
    final rho = await vrfCalculator.rhoForSlot(slot, eta);
    final isLeader =
        await leaderElectionValidation.isSlotLeaderForThreshold(threshold, rho);
    if (isLeader) {
      final evidence = threshold.thresholdEvidence;
      final testProof = await vrfCalculator.proofForSlot(slot, eta);
      final cert = EligibilityCertificate(
          vrfSig: testProof,
          vrfVK: vkVrf,
          thresholdEvidence: evidence,
          eta: eta);
      return VrfHit(cert, slot, threshold);
    }
    return null;
  }
}
