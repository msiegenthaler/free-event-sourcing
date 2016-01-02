package example.account

import slfes.{ProcessType, ProcessBody}
import slfes.syntax.ProcessSyntax._
import Aggregates._
import Transaction.Event._
import Account.Command._

object TransactionResultProcess {
  val description = ProcessType.create(transaction)(spawn, start)

  def spawn(event: transaction.Event): Option[Transaction.Id] = {
    ???
  }

  def start(id: Transaction.Id): ProcessBody = {
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