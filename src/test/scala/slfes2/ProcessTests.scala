package slfes2

import shapeless.{ HNil, :: }
import slfes2.accountprocessing.{ Account, AccountProcessing }
import slfes2.accountprocessing.Account.Event.{ Closed, Opened }

object ProcessTests {
  val selectorOpened = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  val selectorClosed = AggregateEventSelector(Account)(Account.Id(1))[Closed]

  val process = new Process[AccountProcessing.type] {}
  import process.ProcessAction._

  def sel[S <: EventSelector.WithEventType](s: S)(implicit selector: EventSelector[S]) = selector
  val s = sel(selectorOpened)
  val b = s.castEvent(???)
  println(b.owner)

  val select1 = AwaitEvent(selectorOpened)
  val select2 = AwaitEvent(selectorClosed)
  val first = FirstOf(select1, select2)

  //TODO test non compilation of not listed selector
}
