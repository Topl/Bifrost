import 'package:topl_protobuf/node/models/block.pb.dart';

abstract class BlockPackerAlgebra {
  Future<FullBlockBody?> improvePackedBlock(FullBlockBody current);
}
