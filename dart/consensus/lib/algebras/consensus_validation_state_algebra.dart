import 'package:bifrost_common/models/ratio.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';

abstract class ConsensusValidationStateAlgebra {
  Future<Ratio?> operatorRelativeStake(
      BlockId currentBlockId, int slot, StakingAddress address);

  Future<SignatureKesProduct?> operatorRegistration(
      BlockId currentBlockId, int slot, StakingAddress address);
}
