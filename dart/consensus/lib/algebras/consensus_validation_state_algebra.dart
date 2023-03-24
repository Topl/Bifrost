import 'package:rational/rational.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/operational_certificate.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';
import 'package:fixnum/fixnum.dart';

abstract class ConsensusValidationStateAlgebra {
  Future<Rational?> operatorRelativeStake(
      BlockId currentBlockId, Int64 slot, StakingAddress address);

  Future<SignatureKesProduct?> operatorRegistration(
      BlockId currentBlockId, Int64 slot, StakingAddress address);
}
