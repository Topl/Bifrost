import 'package:fpdart/fpdart.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';

abstract class BlockHeadervalidationAlgebra {
  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  Future<List<String>> validate(BlockHeader header);
}
