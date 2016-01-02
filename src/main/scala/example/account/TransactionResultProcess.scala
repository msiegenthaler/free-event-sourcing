package example.account

import slfes.Process
import slfes.syntax.ProcessSyntax._
import Aggregates._
import Transaction.Event._
import Account.Command._

class TransactionResultProcess {
  def start(id: Transaction.Id): Process[Unit] = {
    awaitM(from(id)
      .event[Confirmed](confirm(id))
      .event[Canceled](cancel(id)))
  }

  def confirm(id: Transaction.Id)(e: Confirmed) = for {
    _ ← execute(e.from, ConfirmTransaction(id))
    _ ← execute(e.to, ConfirmTransaction(id))
  } yield ()

  def cancel(id: Transaction.Id)(e: Canceled) = for {
    _ ← execute(e.from, AbortTransaction(id))
    _ ← execute(e.to, AbortTransaction(id))
  } yield ()
}