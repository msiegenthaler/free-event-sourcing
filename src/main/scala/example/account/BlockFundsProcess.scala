package example.account

import slfes.ProcessBody
import slfes.syntax.ProcessSyntax._
import Account.Command._
import Transaction.Event.Created
import AccountProcessing._

/** Blocks the funds in the debited account and confirms/aborts the tx based on the result.
 *  Updating the accounts then happens in the separate TransactionResultProcess.
 */
object BlockFundsProcess {
  val definition = process("blockFunds").startedAt(transaction)
    .on[Created].withMetadata(_.from)
    .withBody(body)

  private def body(id: Transaction.Id): ProcessBody = for {
    c ← await(from(id).event[Created](identity))
    _ ← execute(c.from, BlockFunds(id, c.amount))
    _ ← execute(c.to, AnnounceDeposit(id, c.amount))
    //TODO handle the command results => If fails then abort
    _ ← awaitBlocking(id, c.from)
    _ ← execute(id, Transaction.Command.Confirm())
  } yield ()

  private def awaitBlocking(tx: Transaction.Id, acc: Account.Id): ProcessBody = for {
    candidate ← await(from(acc).event[Account.Event.Blocked](_.by))
    // TODO maybe abort after some time?
    _ ← if (candidate == tx) noop else awaitBlocking(tx, acc)
  } yield ()
}