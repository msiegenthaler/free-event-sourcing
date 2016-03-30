package slfes2.accountprocessing

import shapeless.HNil
import slfes2.BoundedContext

object AccountProcessing {
  private implicit val implementations = AccountImplementation

  val boundedContext = BoundedContext(Account :: HNil)
}
