package slfes2.akka

import org.scalatest.{ FlatSpec, Matchers }
import slfes2.accountprocessing.{ Account, AccountProcessing, Transaction }

class SupportedBoundedContextTests extends FlatSpec with Matchers {

  "SupportedBoundedContext " should " auto derive from a BoundedContextImplementation" in {
    implicit val bci = slfes2.accountprocessing.impl.accountProcessing
    val sbc = implicitly[SupportedBoundedContext[AccountProcessing.type]]
    sbc.aggregates.get(Account) shouldBe defined
    sbc.aggregates.get(Transaction) shouldBe defined
  }
}
