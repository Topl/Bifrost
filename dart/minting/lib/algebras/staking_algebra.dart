import 'package:bifrost_common/models/unsigned.dart';
import 'package:bifrost_minting/models/vrf_hit.dart';
import 'package:fixnum/fixnum.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/consensus/models/staking_address.pb.dart';

abstract class StakingAlgebra {
  StakingAddress get address;
  Future<VrfHit?> elect(SlotId parentSlotId, Int64 slot);
  Future<BlockHeader?> certifyBlock(
      SlotId parentSlotId,
      Int64 slot,
      UnsignedBlockHeader Function(PartialOperationalCertificate)
          unsignedBlockBuilder);
}
