package freeeventsourcing.akka

import org.scalatest.{ FlatSpec, Matchers }
import freeeventsourcing.accountprocessing.{ Account, AccountProcessing, Transaction }

class SupportedBoundedContextTests extends FlatSpec with Matchers {

  "SupportedBoundedContext " should " auto derive from a BoundedContextImplementation" in {
    implicit val bci = freeeventsourcing.accountprocessing.impl.accountProcessing
    val sbc = implicitly[SupportedBoundedContext[AccountProcessing.type]]
    sbc.aggregates.get(Account) shouldBe defined
    sbc.aggregates.get(Transaction) shouldBe defined
  }
}
