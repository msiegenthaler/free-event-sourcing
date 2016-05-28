package freeeventsourcing

import freeeventsourcing.accountprocessing.Account.Command.BlockFunds
import freeeventsourcing.accountprocessing.Account.Error.{ InsufficientFunds, NotOpen }
import freeeventsourcing.accountprocessing.Transaction.Event.Created
import freeeventsourcing.accountprocessing.{ Account, AccountProcessing, Transaction }
import freeeventsourcing.syntax.ProcessSyntax
import org.scalatest.{ FlatSpec, Matchers }

class ProcessDefinitionTests extends FlatSpec with Matchers {
  val syntax = new ProcessSyntax(AccountProcessing)
  import syntax._

  "ProcessDefinition " should " allow to define a process" in {
    //TODO selector for all Open events...
    val sel = AggregateEventSelector(Transaction)(Transaction.Id(1))[Created]
    ProcessDefinition(AccountProcessing, "BlockFunds")(sel) { created ⇒
      for {
        _ ← on(created.event.from).execute(BlockFunds(???, created.event.amount))(
          _.catching[InsufficientFunds](_ ⇒ terminate).
            catching[NotOpen](_ ⇒ terminate)
        )
      } yield ()
    }
  }

}
