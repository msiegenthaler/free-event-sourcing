package slfes2.accountprocessing

import shapeless.HNil
import slfes2.BoundedContext
import slfes2.accountprocessing.impl._

object AccountProcessing {
  val boundedContext = BoundedContext(Account :: HNil)
}
