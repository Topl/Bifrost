import 'dart:isolate';

import 'package:ffi/ffi.dart';
import 'package:flutter/material.dart';

import 'dart:ffi' as ffi;

import 'package:flutter_node/bifrost-node-bindings.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: const MyHomePage(title: 'Flutter Demo Home Page'),
    );
  }
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({super.key, required this.title});
  final String title;

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

launchNodeViaLibrary(int arg) {
  final lib = ffi.DynamicLibrary.open("lib/library/bifrost-node-tetra.so");
  final library = NativeLibrary(lib);
  final args = <String>["bifrost", "--p2pBindHost", "0.0.0.0"];
  final List<ffi.Pointer<ffi.Char>> argPointers =
      args.map((s) => s.toNativeUtf8().cast<ffi.Char>()).toList();
  final ffi.Pointer<ffi.Pointer<ffi.Char>> pointerPointer =
      malloc.allocate(args.length);
  for (int i = 0; i < args.length; i++) {
    pointerPointer[i] = argPointers[i];
  }
  print("Running node");
  library.run_main(args.length, pointerPointer);
  print("Run complete");
}

class _MyHomePageState extends State<MyHomePage> {
  bool _starting = false;
  bool _running = false;

  _runNode() async {
    setState(() {
      _starting = true;
    });
    await Isolate.spawn(launchNodeViaLibrary, 0);
    setState(() {
      _running = true;
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(widget.title),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: _running
              ? <Widget>[
                  const Text(
                    'Node is running',
                  )
                ]
              : <Widget>[
                  TextButton(
                      onPressed: () => _runNode(),
                      child: const Text(
                        'Start node',
                      )),
                ],
        ),
      ),
    );
  }

  _body(BuildContext context) {
    if (!_starting) {
      return Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: <Widget>[
          TextButton(
              onPressed: () => _runNode(),
              child: const Text(
                'Start node',
              )),
        ],
      );
    } else if (!_running) {
      return Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: <Widget>[
          Text(
            'Node is starting',
          )
        ],
      );
    } else {
      return Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: <Widget>[
          Text(
            'Node is running',
          )
        ],
      );
    }
  }
}
