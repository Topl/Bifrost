import com.orientechnologies.orient.core.db.{ODatabase, ODatabaseSession, OrientDB, OrientDBConfig}
import com.orientechnologies.orient.core.record.ORecord

val db: OrientDB = new OrientDB("memory:test", OrientDBConfig.defaultConfig())
val session: ODatabaseSession = db.open("db", "admin", "admin")

try {
  val dbRecords: ODatabase[ORecord] = session.begin()

  dbRecords.

  dbRecords.commit()
} finally
session.close()

//

import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx

val db2 = new ODatabaseDocumentTx("memory:test")
db2.open("admin", "admin")

db2.begin()
try {
  db2.commit()
  // YOUR CODE

  db2

  db2.commit()

} finally db2.close()

//

import com.orientechnologies.orient.core.tx.OTransaction
import com.orientechnologies.orient.core.tx.OTransaction.TXTYPE

db.open("remote:localhost:7777/petshop");

try {
  db.begin(TXTYPE.OPTIMISTIC)



  db.commit();
} catch {
  case _: Exception => db.rollback();
} finally{
  db.close();
}
