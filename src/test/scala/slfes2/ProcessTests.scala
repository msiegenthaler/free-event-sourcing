package slfes2

import slfes2.Process.ProcessAction.{ Await, FirstOf, AwaitEvent }
import slfes2.accountprocessing.Account
import slfes2.accountprocessing.Account.Event.{ Closed, Opened }

object ProcessTests {

  val selectorOpened = AggregateEventSelector(Account)(Account.Id(1))[Opened]
  val selectorClosed = AggregateEventSelector(Account)(Account.Id(1))[Closed]

  def sel[S <: EventSelector.WithEventType](s: S)(implicit selector: EventSelector[S]) = selector
  val s = sel(selectorOpened)
  val b = s.castEvent(???)
  println(b.owner)

  val select1 = AwaitEvent(selectorOpened)
  val select2 = AwaitEvent(selectorClosed)
  val first = FirstOf(select1, select2)
}
