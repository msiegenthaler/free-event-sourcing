package freeeventsourcing.accountprocessing.impl

import java.time.temporal.ChronoUnit
import cats.syntax.all._
import freeeventsourcing._
import freeeventsourcing.EventTime.Zero
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
object BlockFundsProcess extends ProcessHelper(AccountProcessing, "BlockFundsProcess")(AggregateTypeEventSelector(Transaction)[Created]) {
  protected[this] case class Instance(created: Event) extends ProcessInstance {
    import syntax._
    def txId = created.aggregate
    def tx = created.event
    val completeUntil = created.metadata.time.when.plus(1, ChronoUnit.DAYS)

    def process = blockMoney(Zero) >> announceMoney(Zero) >> confirm

    /** Block the money in the account to be debited. */
    def blockMoney(time: EventTime): ProcessMonad[Unit] = {
      def wait = firstOf(_.
        from(tx.from).when[BalanceChanged].after(time).select.flatMapMetadata(e ⇒ blockMoney(e.metadata.time)).
        from(tx.from).when[TxAborted].after(time).select.flatMapMetadata(e ⇒ blockMoney(e.metadata.time)).
        from(tx.from).when[Opened].after(time).select.flatMapMetadata(e ⇒ blockMoney(e.metadata.time)).
        timeout(completeUntil)(abort))

      on(tx.from).execute(BlockFunds(txId, tx.amount))(_.
        catched[InsufficientFunds](wait).
        catched[NotOpen](wait))
    }

    /** Announce the pending deposit to the receiving account. */
    def announceMoney(time: EventTime): ProcessMonad[Unit] = {
      def wait = firstOf(_.
        from(tx.to).on[Opened].flatMapMetadata(e ⇒ announceMoney(e.metadata.time)).
        timeout(completeUntil)(abort))

      on(tx.to).execute(AnnounceDeposit(txId, tx.amount))(_.
        catched[NotOpen](wait))
    }

    /** Wait for confirmation from the debited account and then confirm the transaction */
    def confirm = for {
      _ ← firstOf(_.
        from(tx.from).when[Blocked].matches(_.event.by == txId).select.event.
        timeout(completeUntil)(abort))
      _ ← on(txId).execute(Confirm())(_.
        terminateOn[DoesNotExist].
        terminateOn[AlreadyCanceled])
    } yield ()

    /** Abort the transaction and terminate the process. */
    def abort = for {
      _ ← on(txId).execute(Cancel())(_.
        terminateOn[AlreadyConfirmed].
        terminateOn[DoesNotExist])
      _ ← terminate
    } yield ()
  }
}