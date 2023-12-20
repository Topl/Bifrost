import 'package:admin/ui/bifrost_admin_home_page.dart';
import 'package:flutter/material.dart';

void main() {
  runApp(const BifrostAdminApp());
}

class BifrostAdminApp extends StatelessWidget {
  const BifrostAdminApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
        title: 'Bifrost Admin',
        theme: ThemeData(
          primarySwatch: Colors.green,
        ),
        home: const BifrostAdminHomePage(
          title: 'Bifrost Admin',
        ));
  }
}
