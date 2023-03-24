import 'package:bifrost_codecs/codecs.dart';
import 'package:bifrost_common/algebras/clock_algebra.dart';
import 'package:bifrost_consensus/algebras/block_header_validation_algebra.dart';
import 'package:bifrost_consensus/algebras/consensus_validation_state_algebra.dart';
import 'package:bifrost_consensus/algebras/eta_calculation_algebra.dart';
import 'package:bifrost_consensus/algebras/leader_election_validation_algebra.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';

class BlockHeaderValidation extends BlockHeadervalidationAlgebra {
  final BlockId bigBangBlockId;
  final EtaCalculationAlgebra etaInterpreter;
  final ConsensusValidationStateAlgebra consensusValidationState;
  final LeaderElectionValidationAlgebra leaderElectionValidation;
  final ClockAlgebra clock;
  final Future<BlockHeader> Function(BlockId) fetchHeader;
  @override
  Future<List<String>> validate(BlockHeader header) async {
    if (header.id == bigBangBlockId) return [];
    final parent = await fetchHeader(header.parentHeaderId);
    final List<String> errors = [];

    errors.addAll(_statelessVerification(header, parent));
    if (errors.isNotEmpty) return errors;

    errors.addAll(_timeSlotVerification(header));
    if (errors.isNotEmpty) return errors;

    // TODO Remainder

    return [];
  }

  List<String> _statelessVerification(BlockHeader child, BlockHeader parent) {
    if (child.slot > parent.slot) return ["NonForwardSlot"];
    if (child.timestamp > parent.timestamp) return ["NonForwardTimestamp"];
    if (child.height != parent.height + 1) return ["NonForwardHeight"];
    return [];
  }

  List<String> _timeSlotVerification(BlockHeader header) {
    if (clock.timestampToSlot(header.timestamp) != header.slot)
      return ["TimestampSlotMismatch"];
    if (header.slot < (clock.globalSlot + clock.forwardBiasedSlotWindow))
      return ["SlotBeyondForwardBiasedSlotWindow"];
    return [];
  }
}
