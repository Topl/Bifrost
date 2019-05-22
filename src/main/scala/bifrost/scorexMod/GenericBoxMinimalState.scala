package bifrost.scorexMod

/**
  * Created by cykoz on 4/13/17.
  */

import bifrost.PersistentNodeViewModifier
import bifrost.transaction.box.proposition.Proposition

import scala.util.Try


trait GenericBoxMinimalState[T, P <: Proposition,
BX <: GenericBox[P,T],
BTX <: GenericBoxTransaction[P, T, BX],
M <: PersistentNodeViewModifier[P, BTX],
BMS <: GenericBoxMinimalState[T, P, BX, BTX, M, BMS]]
  extends GenericMinimalState[T, P, BX, BTX, M, BMS] {
  self: BMS =>

  /**
    * A transaction is valid against a state if:
    * - boxes a transaction is opening are stored in the state as closed
    * - sum of values of closed boxes = sum of values of open boxes - fee
    * - all the signatures for open boxes are valid(against all the txs bytes except of sigs)
    *
    * - fee >= 0
    *
    * specific semantic rules are applied
    *
    * @param tx - transaction to check against the state
    * @return
    */
  /* override def validate(tx: BTX): Try[Unit] = {
    val statefulValid = {
      val boxesSumTry = tx.unlockers.foldLeft[Try[Long]](Success(0L)) { case (partialRes, unlocker) =>
        partialRes.flatMap { partialSum =>
          closedBox(unlocker.closedBoxId) match {
            case Some(box) =>
              unlocker.boxKey.isValid(box.proposition, tx.messageToSign) match {
                case true => Success(partialSum + box.value)
                case false => Failure(new Exception("Incorrect unlocker"))
              }
            case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
          }
        }
      }

      boxesSumTry flatMap { openSum =>
        tx.newBoxes.map(_.value).sum == openSum - tx.fee match {
          case true => Success[Unit](Unit)
          case false => Failure(new Exception("Negative fee"))
        }
      }
    }
    statefulValid.flatMap(_ => semanticValidity(tx))
  }*/

  def semanticValidity(tx: BTX): Try[Unit]
}

/*
todo: rewrite / uncomment
trait AuthenticatedBoxMinimalState[P <: Proposition, BX <: Box[P], TX <: Transaction[P], M <: PersistentNodeViewModifier[P, TX], HashFunction <: CryptographicHash,
AMS <: AuthenticatedBoxMinimalState[P, BX, TX, M, HashFunction, AMS]]
  extends MinimalState[P, BX, TX, M, AMS] {
  self: AMS =>

  type ElementProof <: DataProof
  type Storage <: StorageType

  protected val boxesStorage: AuthenticatedDictionary[ElementProof, Storage]

  def digest: HashFunction#Digest

  val hashFunction: HashFunction
}*/
