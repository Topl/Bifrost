import 'package:topl_protobuf/node/models/block.pb.dart';

abstract class BlockProducerAlgebra {
  Future<Stream<FullBlock>> get blocks;
}
