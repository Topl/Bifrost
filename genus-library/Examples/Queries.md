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

Ex3: Relationship between headers and body and transaction, for an existing BlockHeader with id 26:0 (id:cluster)

```roomsql
MATCH {Class: Transaction}-hasTxIO-{Class: BlockHeader, where: (@rid=26:0)}-hasBody-{Class: BlockBody}
RETURN $pathelements
```