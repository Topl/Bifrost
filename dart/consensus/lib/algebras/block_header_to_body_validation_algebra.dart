import 'package:topl_protobuf/node/models/block.pb.dart';

abstract class BlockHeaderToBodyValidationAlgebra {
  Future<List<String>> validate(Block block);
}
