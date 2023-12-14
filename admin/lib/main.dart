import 'dart:math';

import 'package:async/async.dart';
import 'package:flutter/material.dart';
import 'package:graphview/GraphView.dart';
import 'package:fast_base58/fast_base58.dart';
import 'package:topl_common/genus/services/node_grpc.dart';
import 'package:topl_common/proto/consensus/models/block_header.pb.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
        title: 'Flutter Demo',
        theme: ThemeData(
          primarySwatch: Colors.green,
        ),
        home: MyHomePage(
          title: 'Bifrost',
          clients: [NodeGRPCService(host: "localhost", port: 9084)],
        ));
  }
}

class MyHomePage extends StatelessWidget {
  const MyHomePage({Key? key, required this.title, required this.clients})
      : super(key: key);

  final String title;
  final List<NodeGRPCService> clients;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
      ),
      body: Center(
        child: InteractiveViewer(
          constrained: false,
          minScale: 0.1,
          maxScale: 100,
          child: Container(
              // width: 5000,
              // height: 5000,
              child: BlockchainGraph(clients: clients)),
        ),
      ),
    );
  }
}

class BlockchainGraph extends StatefulWidget {
  final List<NodeGRPCService> clients;

  const BlockchainGraph({super.key, required this.clients});

  @override
  State<StatefulWidget> createState() {
    return BlockchainGraphState();
  }
}

class BlockchainGraphState extends State<BlockchainGraph> {
  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return StreamBuilder<Graph>(
      stream: _collectHeaders(),
      builder: (context, snapshot) => snapshot.hasData
          ? GraphView(
              graph: snapshot.data!,
              // algorithm: FruchtermanReingoldAlgorithm(
              //     repulsionPercentage: 0.6, repulsionRate: 0.7),
              algorithm: SugiyamaAlgorithm(SugiyamaConfiguration()
                ..orientation = SugiyamaConfiguration.ORIENTATION_RIGHT_LEFT),
              paint: Paint()
                ..color = Colors.green
                ..strokeWidth = 1
                ..style = PaintingStyle.stroke,
              builder: (node) => Container(
                  width: 10,
                  height: 10,
                  decoration: const BoxDecoration(
                      color: Colors.blue, shape: BoxShape.circle)),
            )
          : const Text("No Data"),
    );
  }

  Stream<Graph> _collectHeaders() async* {
    final Graph graph = Graph()..isTree = true;
    Map<String, Node> nodes = {};
    Map<String, int> heights = {};
    int maxHeight = 0;

    final stream = StreamGroup.merge(widget.clients.map((client) => client
        .synchronizationTraversal()
        .where((res) => res.hasApplied())
        .map((res) => (res.applied, client))));

    await for (final tuple in stream) {
      final client = tuple.$2;
      var id = tuple.$1;
      var idString = Base58Encode(id.value);
      var foundGenesis = false;
      // Fetch _this_ many missing ancestors of the received block
      var count = 4;
      List<(String, BlockHeader)> missing = [];
      while (!nodes.containsKey(idString) && !foundGenesis && count > 0) {
        final headerRes = await client.fetchBlockHeader(
            blockIdBytes: id.value, options: null);
        if (headerRes.hasHeader()) {
          missing.add((idString, headerRes.header));
          id = headerRes.header.parentHeaderId;
          idString = Base58Encode(id.value);
        } else {
          foundGenesis = true;
        }
        count--;
      }
      for (final missingTuple in missing.reversed) {
        final idString = missingTuple.$1;
        final header = missingTuple.$2;
        final parentIdString = Base58Encode(header.parentHeaderId.value);
        final node = Node.Id(idString);
        node.x = 250;
        node.y = 250;
        nodes[idString] = node;
        final height = header.height.toInt();
        heights[idString] = height;
        maxHeight = max(height, maxHeight);
        graph.addNode(node);
        if (nodes.containsKey(parentIdString)) {
          final parentNode = nodes[parentIdString]!;
          graph.addEdge(node, parentNode);
        }
      }
      if (missing.isNotEmpty) {
        final toRemove = heights.entries
            .where((element) => element.value < (maxHeight - 10))
            .toList();
        for (final entry in toRemove) {
          heights.remove(entry.key);
          graph.removeNode(nodes[entry.key]);
          nodes.remove(entry.key);
        }
        yield graph;
      }
    }
  }
}
