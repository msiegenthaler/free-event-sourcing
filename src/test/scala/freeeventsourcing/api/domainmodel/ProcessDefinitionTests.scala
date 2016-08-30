package freeeventsourcing.api.domainmodel

import freeeventsourcing.accountprocessing.Account.Command.BlockFunds
import freeeventsourcing.accountprocessing.Account.Error.{ InsufficientFunds, NotOpen }
import freeeventsourcing.accountprocessing.Transaction.Event.Created
import freeeventsourcing.accountprocessing.{ AccountProcessing, Transaction }
import freeeventsourcing.api.domainmodel.eventselector.AggregateTypeEventSelector
import freeeventsourcing.syntax.ProcessSyntax
import org.scalatest.{ FlatSpec, Matchers }

class ProcessDefinitionTests extends FlatSpec with Matchers {
  val syntax = new ProcessSyntax(AccountProcessing)
  import syntax._

  "ProcessDefinition " should " allow to define a process" in {
    val sel = AggregateTypeEventSelector(Transaction)[Created]
    ProcessDefinition(AccountProcessing, "BlockFunds")(sel) { created ⇒
      for {
        _ ← on(created.event.from).execute(BlockFunds(created.aggregate, created.event.amount))(
          _.catching[InsufficientFunds](_ ⇒ terminate).
            catching[NotOpen](_ ⇒ terminate)
        )
      } yield ()
    }
  }

}
