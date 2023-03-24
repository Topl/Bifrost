import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:bifrost_p2p/algebras/ServerAlgebra.dart';
import 'package:topl_protobuf/brambl/models/evidence.pb.dart';
import 'package:topl_protobuf/brambl/models/identifier.pb.dart';
import 'package:topl_protobuf/brambl/models/transaction/io_transaction.pb.dart';
import 'package:topl_protobuf/consensus/models/block_header.pb.dart';
import 'package:topl_protobuf/consensus/models/block_id.pb.dart';
import 'package:topl_protobuf/node/models/block.pb.dart';
import 'package:topl_protobuf/quivr/models/shared.pb.dart';

class P2PServer extends ServerAlgebra {
  final String p2pId;
  final String bindHost;
  final int bindPort;
  final Stream<String> _knownPeers;
  final void Function(Socket) handleSocket;
  late ServerSocket? _server;

  P2PServer(
    this.p2pId,
    this.bindHost,
    this.bindPort,
    this._knownPeers,
    this.handleSocket,
  );

  @override
  Future<void> start() async {
    _server = await ServerSocket.bind(bindHost, bindPort);
    _server!.listen(handleSocket);
    _knownPeers.asyncMap((address) async {
      final args = address.split(":");
      final socket = await Socket.connect(args[0], int.parse(args[1]));
      handleSocket(socket);
    }).drain();
  }

  @override
  Future<void> stop() async {
    if (_server != null) await _server!.close();
  }
}

class DataGossipSocketHandler {
  final Socket socket;
  final Future<Data?> Function(DataRequest) fulfillRemoteRequest;
  final void Function(DataNotification) dataNotificationReceived;

  final _pendingRequests = <DataRequest, Completer<Data?>>{};

  DataGossipSocketHandler(
      this.socket, this.fulfillRemoteRequest, this.dataNotificationReceived) {
    _processSocket();
  }

  void notifyData(DataNotification notification) {
    final message = Uint8List.fromList([0, notification.typeByte])
      ..addAll(notification.id);
    socket.add(_createSocketFrame(message));
  }

  Future<Data?> requestData(DataRequest request) {
    final message = Uint8List.fromList([1, request.typeByte])
      ..addAll(request.id);
    socket.add(_createSocketFrame(message));
    final completer = Completer<Data?>();
    _pendingRequests[request] = completer;
    return completer.future;
  }

  _createSocketFrame(Uint8List bytes) =>
      _intToBytes(bytes.length)..addAll(bytes);

  ParsedSocketFrame? _parseSocketFrame(Uint8List buffer) {
    if (buffer.length >= 4) {
      final length = _bytesToInt(buffer.sublist(0, 4));
      if (buffer.length >= (4 + length)) {
        return ParsedSocketFrame(buffer.sublist(4, buffer.length + 4),
            buffer.sublist(buffer.length + 4));
      }
    }
    return null;
  }

  void _processSocket() {
    var buffer = Uint8List.fromList([]);
    socket.forEach((data) {
      buffer.addAll(data);
      var maybeFrame = _parseSocketFrame(buffer);
      if (maybeFrame != null) {
        buffer.clear();
        buffer.addAll(maybeFrame.remaining);
        _processFrame(maybeFrame);
        maybeFrame = _parseSocketFrame(buffer);
      }
    });
  }

  void _processFrame(ParsedSocketFrame frame) {
    switch (frame.data[0]) {
      case 0:
        final notification =
            DataNotification(frame.data[1], frame.data.sublist(2));
        dataNotificationReceived(notification);
        break;
      case 1:
        final request = DataRequest(frame.data[1], frame.data.sublist(2));
        fulfillRemoteRequest(request).then((maybeData) {
          if (maybeData != null) {
            final responseMessage = Uint8List.fromList([maybeData.typeByte])
              ..addAll(_intToBytes(maybeData.id.length))
              ..addAll(maybeData.id)
              ..add(1)
              ..addAll(maybeData.data);
            final responseFrame = _createSocketFrame(responseMessage);
            socket.add(responseFrame);
          } else {
            final responseMessage = Uint8List.fromList([request.typeByte])
              ..addAll(request.id)
              ..add(0);
            final responseFrame = _createSocketFrame(responseMessage);
            socket.add(responseFrame);
          }
        });
        break;
      case 3:
        final typeByte = frame.data[1];
        final idLength = _bytesToInt(frame.data.sublist(2, 6));
        final id = frame.data.sublist(6, idLength + 6);
        final hasData = frame.data[idLength + 6] == 1;
        final maybeData = hasData
            ? Data(typeByte, id, frame.data.sublist(idLength + 7))
            : null;
        _pendingRequests[DataRequest(typeByte, id)]?.complete(maybeData);
        break;
    }
  }

  // TODO
  Uint8List _intToBytes(int value) => throw UnimplementedError();
  // TODO
  int _bytesToInt(Uint8List bytes) => throw UnimplementedError();
}

class ParsedSocketFrame {
  final Uint8List data;
  final Uint8List remaining;

  ParsedSocketFrame(this.data, this.remaining);
}

class DataRequest {
  final int typeByte;
  final Uint8List id;

  DataRequest(this.typeByte, this.id);
}

class DataNotification {
  final int typeByte;
  final Uint8List id;

  DataNotification(this.typeByte, this.id);
}

class DataResponse {
  final int typeByte;
  final Uint8List id;
  final Uint8List? data;

  DataResponse(this.typeByte, this.id, this.data);
}

class Data {
  final int typeByte;
  final Uint8List id;
  final Uint8List data;

  Data(this.typeByte, this.id, this.data);
}

class BlockchainDataGossipHandler {
  final Socket socket;
  late final DataGossipSocketHandler socketHandler;

  BlockchainDataGossipHandler(
    this.socket,
    void Function(BlockId) blockIdNotified,
    void Function(Identifier_IoTransaction32) transactionIdNotified,
    Future<BlockHeader?> Function(BlockId) fetchLocalHeader,
    Future<BlockBody?> Function(BlockId) fetchLocalBlockBody,
    Future<IoTransaction?> Function(Identifier_IoTransaction32)
        fetchLocalTransaction,
  ) {
    Future<Data?> fulfillRequest(DataRequest request) async {
      switch (request.typeByte) {
        case 10:
          final header = await fetchLocalHeader(BlockId(value: request.id));
          if (header != null) {
            return Data(request.typeByte, request.id, header.writeToBuffer());
          }
          break;
        case 11:
          final transaction = await fetchLocalTransaction(
              Identifier_IoTransaction32(
                  evidence: Evidence_Sized32(
                      digest: Digest_Digest32(value: request.id))));
          if (transaction != null) {
            return Data(
                request.typeByte, request.id, transaction.writeToBuffer());
          }
          break;
        case 12:
          final body = await fetchLocalBlockBody(BlockId(value: request.id));
          if (body != null) {
            return Data(request.typeByte, request.id, body.writeToBuffer());
          }
          break;
      }
      return null;
    }

    void notificationReceived(DataNotification notification) {
      switch (notification.typeByte) {
        case 10:
          blockIdNotified(BlockId(value: notification.id));
          break;
        case 11:
          transactionIdNotified(Identifier_IoTransaction32(
              evidence: Evidence_Sized32(
                  digest: Digest_Digest32(value: notification.id))));
          break;
      }
    }

    this.socketHandler =
        DataGossipSocketHandler(socket, fulfillRequest, notificationReceived);
  }

  Future<BlockHeader?> requestHeader(BlockId id) async {
    final request = DataRequest(10, Uint8List.fromList(id.value));
    final maybeData = await (socketHandler.requestData(request));
    if (maybeData != null) return BlockHeader.fromBuffer(maybeData.data);
    return null;
  }

  Future<BlockBody?> requestBody(BlockId id) async {
    final request = DataRequest(12, Uint8List.fromList(id.value));
    final maybeData = await (socketHandler.requestData(request));
    if (maybeData != null) return BlockBody.fromBuffer(maybeData.data);
    return null;
  }

  Future<IoTransaction?> requestTransaction(
      Identifier_IoTransaction32 id) async {
    final request =
        DataRequest(11, Uint8List.fromList(id.evidence.digest.value));
    final maybeData = await (socketHandler.requestData(request));
    if (maybeData != null) return IoTransaction.fromBuffer(maybeData.data);
    return null;
  }

  void notifyBlockId(BlockId id) {
    final notification = DataNotification(10, Uint8List.fromList(id.value));
    socketHandler.notifyData(notification);
  }

  void notifyTransactionId(Identifier_IoTransaction32 id) {
    final notification =
        DataNotification(11, Uint8List.fromList(id.evidence.digest.value));
    socketHandler.notifyData(notification);
  }
}
