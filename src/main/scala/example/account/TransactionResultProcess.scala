package example.account

import slfes._
import slfes.syntax.ProcessSyntax._
import Transaction.Event._
import Account.Command._
import example.account.AccountProcessing._

/** Updates the account when txs are confirmed or aborted. */
object TransactionResultProcess {
  val definition = process("transactionResult").startedAt(transaction)
    .on[Created].withMetadata(_.from)
    .withBody(body)

  private def body(id: Transaction.Id): ProcessBody = awaitM(from(id)
    .event[Confirmed](confirm(id))
    .event[Canceled](cancel(id)))

  private def confirm(id: Transaction.Id)(e: Confirmed) = for {
    _ ← execute(e.from, ConfirmTransaction(id))
    _ ← execute(e.to, ConfirmTransaction(id))
  } yield ()
  private def cancel(id: Transaction.Id)(e: Canceled) = for {
    _ ← execute(e.from, AbortTransaction(id))
    _ ← execute(e.to, AbortTransaction(id))
  } yield ()
}