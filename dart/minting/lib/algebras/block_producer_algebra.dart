import 'package:topl_protobuf/node/models/block.pb.dart';

abstract class BlockProducerAlgebra {
  Stream<FullBlock> get blocks;
}
