package freeeventsourcing.accountprocessing.impl

import java.time.Instant
import freeeventsourcing.accountprocessing.Account.Command.{ AnnounceDeposit, BlockFunds }
import freeeventsourcing.accountprocessing.Account.Error.{ InsufficientFunds, NotOpen }
import freeeventsourcing.accountprocessing.Account.Event._
import freeeventsourcing.{ AggregateEvent, EventWithMetadata, ProcessDefinition }
import freeeventsourcing.accountprocessing.Transaction.Command._
import freeeventsourcing.accountprocessing.Transaction.Error.{ AlreadyCanceled, AlreadyConfirmed, DoesNotExist }
import freeeventsourcing.accountprocessing.Transaction.Event._
import freeeventsourcing.accountprocessing.{ Account, AccountProcessing, Transaction }
import freeeventsourcing.eventselector.AggregateTypeEventSelector
import freeeventsourcing.syntax.ProcessSyntax

/** Blocks the funds in the debited account and confirms/aborts the tx based on the result.
 *  Updating the accounts then happens in the separate TransactionResultProcess.
 */
object BlockFundsProcess {

  val process = ProcessDefinition(AccountProcessing, "BlockFunds")(selector)(new Body(_).main)

  private[this] def selector = AggregateTypeEventSelector(Transaction)[Created]

  private[this] class Body(created: EventWithMetadata[AggregateEvent[Transaction.type, Created]]) {
    val syntax = ProcessSyntax(AccountProcessing)
    import syntax._

    def tx = created.aggregate
    def fromAccount = created.event.from
    def toAccount = created.event.to
    def amount = created.event.amount
    val completeUntil: Instant = ??? //TODO created.metadata.time.realTime?

    def main: ProcessMonad[Unit] = for {
      //Block the money in the from account and announce it to the to account
      _ ← on(fromAccount).execute(BlockFunds(tx, amount))(
        _.catched[InsufficientFunds](waitForDebitedAccount).
          catching[NotOpen](_ ⇒ waitForDebitedAccount)
      )
      _ ← on(toAccount).execute(AnnounceDeposit(tx, amount))(
        _.catched[NotOpen](waitForDepositAccount)
      )

      //Wait for confirmation of the successful blocking of the money in the from account
      _ ← firstOf(
        //TODO we need to be able to filter that to our tx
        _.from(fromAccount).on[Blocked].event.
          timeout(completeUntil)(abortTransaction)
      )

      //Confirm the transaction
      _ ← on(tx).execute(Confirm())(
        _.terminateOn[DoesNotExist].
        terminateOn[AlreadyCanceled]
      )
    } yield ()

    def abortTransaction = for {
      _ ← on(tx).execute(Cancel())(
        _.terminateOn[AlreadyConfirmed].
        terminateOn[DoesNotExist]
      )
      _ ← terminate
    } yield ()

    def waitForDebitedAccount = firstOf(
      //TODO it does not help if we just add another retroactive subscription...
      //TODO we need to start after a specific time, else it triggers instantly
      _.from(fromAccount).on[BalanceChanged].execute(main).
        from(fromAccount).on[TxAborted].execute(main).
        from(fromAccount).on[Opened].execute(main).
        timeout(completeUntil)(abortTransaction)
    ).map(_ ⇒ ()) //TODO allow the methods to accept ProcessMonad[_]

    def waitForDepositAccount: ProcessMonad[Unit] = firstOf(
      _.from(toAccount).on[Opened].execute(main).
        timeout(completeUntil)(abortTransaction)
    ).map(_ ⇒ ()) //TODO allow the methods to accept ProcessMonad[_]
  }

}
