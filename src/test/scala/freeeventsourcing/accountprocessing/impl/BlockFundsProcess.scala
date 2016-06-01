package freeeventsourcing.accountprocessing.impl

import java.time.temporal.ChronoUnit
import freeeventsourcing._
import freeeventsourcing.accountprocessing.Account.Command._
import freeeventsourcing.accountprocessing.Account.Error._
import freeeventsourcing.accountprocessing.Account.Event._
import freeeventsourcing.accountprocessing.Transaction.Command._
import freeeventsourcing.accountprocessing.Transaction.Error._
import freeeventsourcing.accountprocessing.Transaction.Event._
import freeeventsourcing.accountprocessing.{ AccountProcessing, Transaction }
import freeeventsourcing.eventselector.AggregateTypeEventSelector
import freeeventsourcing.syntax.ProcessHelper

/** Blocks the funds in the debited account and confirms/aborts the tx based on the result.
 *  Updating the accounts then happens in the separate TransactionResultProcess.
 */
object BlockFundsProcess extends ProcessHelper(AccountProcessing, "Test")(AggregateTypeEventSelector(Transaction)[Created]) {
  protected[this] case class Instance(created: Event) extends ProcessInstance {
    def process = main(EventTime.Zero)

    import syntax._
    def txId = created.aggregate
    def tx = created.event
    val completeUntil = created.metadata.time.when.plus(1, ChronoUnit.DAYS)

    def main(t: EventTime): ProcessMonad[Unit] = for {
      //Block the money in the from-account and announce it to the to-account
      _ ← on(tx.from).execute(BlockFunds(txId, tx.amount))(_.
        catched[InsufficientFunds](waitForDebitedAccount(t)).
        catched[NotOpen](waitForDebitedAccount(t)))
      _ ← on(tx.to).execute(AnnounceDeposit(txId, tx.amount))(_.
        catched[NotOpen](waitForDepositAccount(t)))

      //Wait for confirmation of the successful blocking of the money in the from account
      _ ← firstOf(_.
        from(tx.from).when[Blocked].matches(_.event.by == txId).select.event.
        timeout(completeUntil)(abortTransaction))

      //Confirm the transaction
      _ ← on(txId).execute(Confirm())(_.
        terminateOn[DoesNotExist].
        terminateOn[AlreadyCanceled])
    } yield ()

    def abortTransaction = for {
      _ ← on(txId).execute(Cancel())(_.
        terminateOn[AlreadyConfirmed].
        terminateOn[DoesNotExist])
      _ ← terminate
    } yield ()

    //TODO the time handling is a bit clumsy, we need to pass that though everywhere
    def waitForDebitedAccount(time: EventTime) = firstOf(_.
      from(tx.from).when[BalanceChanged].after(time).select.flatMapMetadata(e ⇒ main(e.metadata.time)).
      from(tx.from).when[TxAborted].after(time).select.flatMapMetadata(e ⇒ main(e.metadata.time)).
      from(tx.from).when[Opened].after(time).select.flatMapMetadata(e ⇒ main(e.metadata.time)).
      timeout(completeUntil)(abortTransaction))

    def waitForDepositAccount(time: EventTime) = firstOf(_.
      from(tx.to).on[Opened].execute(main(time)).
      timeout(completeUntil)(abortTransaction))
  }
}