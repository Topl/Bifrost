import 'package:grpc/src/server/call.dart';
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/consensus/models/slot_data.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';
import 'package:topl_protobuf/node/services/bifrost_rpc.pbgrpc.dart';

class ToplGrpcService extends NodeRpcServiceBase {
  final Future<BlockHeader?> Function(BlockId) _fetchHeader;
  final Future<BlockBody?> Function(BlockId) _fetchBody;
  final Future<IoTransaction?> Function(Identifier_IoTransaction32)
      _fetchTransaction;
  final Stream<BlockId> Function() _adoptions;

  ToplGrpcService(
    this._fetchHeader,
    this._fetchBody,
    this._fetchTransaction,
    this._adoptions,
  );

  @override
  Future<BroadcastTransactionRes> broadcastTransaction(
      ServiceCall call, BroadcastTransactionReq request) {
    // TODO: implement broadcastTransaction
    throw UnimplementedError();
  }

  @override
  Future<CurrentMempoolRes> currentMempool(
      ServiceCall call, CurrentMempoolReq request) {
    // TODO: implement currentMempool
    throw UnimplementedError();
  }

  @override
  Future<FetchBlockHeaderRes> fetchBlockHeader(
          ServiceCall call, FetchBlockHeaderReq request) async =>
      FetchBlockHeaderRes(header: await _fetchHeader(request.blockId));

  @override
  Future<FetchBlockBodyRes> fetchBlockBody(
          ServiceCall call, FetchBlockBodyReq request) async =>
      FetchBlockBodyRes(body: await _fetchBody(request.blockId));

  @override
  Future<FetchTransactionRes> fetchTransaction(
          ServiceCall call, FetchTransactionReq request) async =>
      FetchTransactionRes(
          transaction: await _fetchTransaction(request.transactionId));

  @override
  Future<FetchBlockIdAtDepthRes> fetchBlockIdAtDepth(
      ServiceCall call, FetchBlockIdAtDepthReq request) {
    // TODO: implement fetchBlockIdAtDepth
    throw UnimplementedError();
  }

  @override
  Future<FetchBlockIdAtHeightRes> fetchBlockIdAtHeight(
      ServiceCall call, FetchBlockIdAtHeightReq request) {
    // TODO: implement fetchBlockIdAtHeight
    throw UnimplementedError();
  }

  @override
  Stream<SynchronizationTraversalRes> synchronizationTraversal(
      ServiceCall call, SynchronizationTraversalReq request) {
    return _adoptions()
        .map((blockId) => SynchronizationTraversalRes(applied: blockId));
  }
}
