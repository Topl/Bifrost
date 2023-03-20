### Genus
- A backend tool for analyzing Topl Blockchain data.
- A service which reads live blockchain data from a Bifrost Node (over gRPC) and persists it in a graph (nodes + edges) format.  
- Graph data is then exposed using a gRPC server. [genus_rpc.proto](https://github.com/Topl/protobuf-specs/blob/main/proto/genus/genus_rpc.proto)

## OrientDB
- OrientDB is a multi-model, NoSQL database with support for graph and document databases [OrientDB](https://orientdb.org/getting-started)
- Genus uses an instance of OrientDB as a database to store its ledger, embedded OrientDB Server [Embedded-Server](https://orientdb.org/docs/3.0.x/internals/Embedded-Server.html)
- 

### Requirements:

- Genus service depends on Bifrost Node Rpc, settings could be found on RPC/gRPC bifrost-node resources folder.

### Optional Requirements:

#### OrientDB Studio: 
Operating on databases through a graphical user interface.

- Download https://orientdb.org/download
- Update `orient-db-directory` configuration file
 
    Example:
   `orient-db-directory ="../../orientdb-community-3.2.17/databases/genus_db"`


```sh
topl cd orientdb-community-3.2.17/bin 
➜ bin sh server.sh
```
OrientDB Studio will be available at http://192.168.1.13:2480/studio/index.html


#### console
```sh
topl cd orientdb-community-3.2.17/bin 
➜ bin sh console.sh
➜ connect plocal:/home/fernando/topl/Bifrost/genus-server/genus_db user pass

orientdb {db=genus_db}> select from BlockHeader order by height limit 40                                           

+----+------+-----------+------+--------+-----------+--------------+----------+--------+----+--------+-------------+----------------------+--------------+----------------------+--------+---------+
|#   |@RID  |@CLASS     |height|txRoot  |bloomFilter|parentHeaderId|parentSlot|metadata|slot|blockId |timestamp    |eligibilityCertificate|StakingAddress|operationalCertificate|in_test2|out_test2|
+----+------+-----------+------+--------+-----------+--------------+----------+--------+----+--------+-------------+----------------------+--------------+----------------------+--------+---------+
|0   |#42:50|BlockHeader|1     |byte[32]|byte[256]  |byte[32]      |-1        |byte[0] |0   |byte[32]|1678992914560|byte[184]             |byte[34]      |byte[377]             |[#43:50]|         |
|1   |#43:50|BlockHeader|2     |byte[32]|byte[256]  |byte[32]      |0         |byte[0] |5   |byte[32]|1678992915759|byte[184]             |byte[34]      |byte[991]             |[#44:48]|[#42:50] |
|2   |#44:48|BlockHeader|3     |byte[32]|byte[256]  |byte[32]      |5         |byte[0] |11  |byte[32]|1678992916959|byte[184]             |byte[34]      |byte[991]             |[#45:48]|[#43:50] |
|3   |#45:48|BlockHeader|4     |byte[32]|byte[256]  |byte[32]      |11        |byte[0] |26  |byte[32]|1678992919959|byte[184]             |byte[34]      |byte[991]             |[#46:39]|[#44:48] |
|4   |#46:39|BlockHeader|5     |byte[32]|byte[256]  |byte[32]      |26        |byte[0] |30  |byte[32]|1678992920759|byte[184]             |byte[34]      |byte[991]             |[#47:28]|[#45:48] |


```

### Know issues

- Locked db, Orient db does not allow multiple tools, (console, studio, ..) at the same time

```
Error: com.orientechnologies.orient.core.exception.OStorageException: Cannot open local storage '/tmp/databases/demo' with mode=rw
com.orientechnologies.common.concur.lock.OLockException: File '/tmp/databases/demo/default.0.oda' is locked by another process, maybe the database is in use by another process. Use the remote mode with a OrientDB server to allow multiple access to the same database
Both errors have the same meaning: a "plocal" database can't be opened by multiple JVM at the same time. To fix:
check if there's no process using OrientDB (most of the times a OrientDB Server is running in the background). Just shutdown that server and retry
if you need multiple access to the same database, don't use "plocal" directly, but rather start a server and access to the database by using "remote" protocol. In this way the server is able to share the same database with multiple clients.
```

- Admin Password warning

 > IMPORTANT! Using default password is unsafe, please change password for user 'admin' on database 'genus_db' [OrientDBDistributed]

More info  https://orientdb.org/docs/1.7.8/orientdb.wiki/Security.html

