package example.account

import shapeless.ops.coproduct._
import shapeless._
import slfes._
import slfes.Process._
import slfes.syntax.ProcessSyntax._

class TransactionResultProcess {
  import Aggregates._

  def xxx(id: Transaction.Id): Process[Unit] = for {
    msg2 ← receive(id)
    m = Transaction.Event.Confirmed(Account.Id(1), Account.Id(2), 1000)
    _ ← execute(m.from, Account.Command.ConfirmTransaction(id))
    _ <- execute(m.from, Account.Command.Close())
  } yield ()

  def yyy(id: Transaction.Id) = {
    import example.account.Transaction.Event._

    val id = Transaction.Id(0)
    val a = await {
      from(id).
        event[Confirmed](_.from).
        event[Canceled](_.from)
    }
  }
}