## Queries 

Ex1: Some block headers ordered
```roomsql
select from BlockHeader order by height limit 20
```

Ex2: Relationship between headers and bodies
```roomsql
MATCH {Class: BlockHeader}-body-{Class: BlockBody}
RETURN $pathelements
```

Ex3: Relationship between headers and body and transaction

```roomsql
select from BlockHeader order by height limit 20
MATCH {Class: Transaction}-txIO-{Class: BlockHeader, where: (@rid=#26:0)}-body-{Class: BlockBody}
RETURN $pathelements
```