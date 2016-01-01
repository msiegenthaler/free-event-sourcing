package example.account

import slfes._
import slfes.Process._

class TransactionResultProcess {
  import Aggregates._

  def xxx(id: Transaction.Id): Process[Unit] = for {
    msg2 ← receive(id)
    m = Transaction.Event.Confirmed(???, ???, ???)
    _ ← execute(m.from, Account.Command.ConfirmTransaction(id))
  } yield ()
}