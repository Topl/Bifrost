## Queries

Ex1: Some block headers ordered
```roomsql
select from BlockHeader order by height limit 20
```

Ex2: Relationship between headers and bodies
```roomsql
MATCH {Class: BlockHeader}-hasBody-{Class: BlockBody}
RETURN $pathelements
```

Ex3: Relationship between headers , body and transaction, for an existing BlockHeader with id 26:0 (id:cluster)

```roomsql
MATCH {Class: Transaction}-hasTxIO-{Class: BlockHeader, where: (@rid=26:0)}-hasBody-{Class: BlockBody}
RETURN $pathelements
```

Ex4: Relationship between headers, body, transaction and address, for an existing BlockHeader with id 26:0 (id:cluster)

```roomsql
MATCH {Class: LockAddress}-hasLockAddress-{Class: Transaction}-hasTxIO-{Class: BlockHeader}
RETURN $pathelements
```

Ex5: Relationship between lockAddress and txo
```roomsql
MATCH {Class: LockAddress}-hasTxo-{Class: Txo}
RETURN $pathelements
```
