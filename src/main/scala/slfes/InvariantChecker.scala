package slfes

import cats.data.Xor
import shapeless.Coproduct

trait InvariantChecker[State, Commands, Events, In <: Inv[State]] extends (Commands ⇒ State ⇒ CmdResult[Events]) {
  def invariants: Traversable[In]
  def applyEvents(events: Seq[Events], state: State): State
  val delegate: Commands ⇒ State ⇒ CmdResult[Events]
  def error(failed: In): Any

  override def apply(cmd: Commands) = state ⇒ {
    delegate(cmd)(state).flatMap { events ⇒
      val s2 = applyEvents(events, state)
      invariants.find(inv ⇒ !inv(state)).
        fold[CmdResult[Events]](Xor.right(events))(failed ⇒ Xor.left(error(failed)))
    }
  }
}

object AggregateWithInvariants {
  def apply[I, S, C <: Coproduct, E <: Coproduct, In <: Inv[S]](
        name: String,
        seed: I ⇒ S,
        handleCommand: C ⇒ S ⇒ CmdResult[E],
        applyEvent: E ⇒ S ⇒ S,
        invariants: Traversable[In],
        onInvariantFailed: In ⇒ Any): AggregateType[I, S, C, E] = {
    val i = invariants
    val handler = new InvariantChecker[S, C, E, In] {
      override def invariants = i
      override def applyEvents(events: Seq[E], state: S) = events.foldLeft(state)((s, e) ⇒ applyEvent(e)(s))
      override val delegate = handleCommand
      override def error(failed: In) = onInvariantFailed(failed)
    }
    AggregateType(name, seed, handler, applyEvent)
  }
}