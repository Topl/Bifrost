import 'package:bifrost_consensus/algebras/block_header_to_body_validation_algebra.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';

class BlockHeaderToBodyValidation extends BlockHeaderToBodyValidationAlgebra {
  @override
  Future<List<String>> validate(Block block) async {
    return []; // TODO
  }
}
