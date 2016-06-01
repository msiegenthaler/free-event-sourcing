package freeeventsourcing.accountprocessing.impl

import freeeventsourcing.accountprocessing.Account.Command._
import freeeventsourcing.accountprocessing.Account.Error._
import freeeventsourcing.accountprocessing.Account.Event._
import freeeventsourcing.accountprocessing.Transaction.Command._
import freeeventsourcing.accountprocessing.Transaction.Error._
import freeeventsourcing.accountprocessing.Transaction.Event._
import freeeventsourcing.accountprocessing.{ AccountProcessing, Transaction }
import freeeventsourcing.eventselector.AggregateTypeEventSelector
import freeeventsourcing.syntax.ProcessSyntax
import freeeventsourcing.{ AggregateEvent, EventTime, EventWithMetadata, ProcessDefinition }

/** Blocks the funds in the debited account and confirms/aborts the tx based on the result.
 *  Updating the accounts then happens in the separate TransactionResultProcess.
 */
object BlockFundsProcess {

  val process = ProcessDefinition(AccountProcessing, "BlockFunds")(selector)(new Body(_).process)

  private[this] def selector = AggregateTypeEventSelector(Transaction)[Created]

  private[this] class Body(created: EventWithMetadata[AggregateEvent[Transaction.type, Created]]) {
    val syntax = ProcessSyntax(AccountProcessing)
    import syntax._

    def tx = created.aggregate
    def fromAccount = created.event.from
    def toAccount = created.event.to
    def amount = created.event.amount
    val completeUntil = created.metadata.time.when

    def process = main(EventTime.Zero)

    def main(t: EventTime): ProcessMonad[Unit] = for {
      //Block the money in the from account and announce it to the to account
      _ ← on(fromAccount).execute(BlockFunds(tx, amount))(_.
        catched[InsufficientFunds](waitForDebitedAccount(t)).
        catched[NotOpen](waitForDebitedAccount(t)))
      _ ← on(toAccount).execute(AnnounceDeposit(tx, amount))(_.
        catched[NotOpen](waitForDepositAccount(t)))

      //Wait for confirmation of the successful blocking of the money in the from account
      _ ← firstOf(_.
        from(fromAccount).when[Blocked].matches(_.event.by == tx).select.event.
        timeout(completeUntil)(abortTransaction))

      //Confirm the transaction
      _ ← on(tx).execute(Confirm())(_.
        terminateOn[DoesNotExist].
        terminateOn[AlreadyCanceled])
    } yield ()

    def abortTransaction = for {
      _ ← on(tx).execute(Cancel())(_.
        terminateOn[AlreadyConfirmed].
        terminateOn[DoesNotExist])
      _ ← terminate
    } yield ()

    //TODO the time handling is a bit clumsy, we need to pass that though everywhere
    def waitForDebitedAccount(time: EventTime) = firstOf(_.
      from(fromAccount).when[BalanceChanged].after(time).select.flatMapMetadata(e ⇒ main(e.metadata.time)).
      from(fromAccount).when[TxAborted].after(time).select.flatMapMetadata(e ⇒ main(e.metadata.time)).
      from(fromAccount).when[Opened].after(time).select.flatMapMetadata(e ⇒ main(e.metadata.time)).
      timeout(completeUntil)(abortTransaction))

    def waitForDepositAccount(time: EventTime) = firstOf(_.
      from(toAccount).on[Opened].execute(main(time)).
      timeout(completeUntil)(abortTransaction))
  }

}
