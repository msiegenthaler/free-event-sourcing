package example.account

import slfes._
import slfes.Process._
import slfes.syntax.ProcessSyntax._

class TransactionResultProcess {
  import Aggregates._

  def xxx(id: Transaction.Id): Process[Unit] = for {
    msg2 ← receive(id)
    accounts ← await(from(id).
      event[Transaction.Event.Confirmed](e ⇒ (e.from, e.to)).
      event[Transaction.Event.Canceled](e ⇒ (e.from, e.to)))
    _ ← execute(accounts._1, Account.Command.ConfirmTransaction(id))
    _ ← execute(accounts._2, Account.Command.ConfirmTransaction(id))
  } yield ()
}