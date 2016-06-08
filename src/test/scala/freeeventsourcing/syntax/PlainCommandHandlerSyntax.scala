package freeeventsourcing.syntax

import scala.language.implicitConversions
import scala.collection.immutable.Seq
import cats.data.Xor
import freeeventsourcing.AggregateCommand
import shapeless.Poly1
import shapeless.ops.coproduct.Inject

/** Syntactic support for writing command handlers. */
trait PlainCommandHandlerSyntax { self: Poly1 ⇒
  protected type Command <: AggregateCommand
  protected type Event
  protected type State

  protected type Result[C <: Command] = C#Error Xor Seq[Event]

  protected[this] def on[C <: Command](f: OnCall[C] ⇒ Result[C]): self.Case.Aux[C, State ⇒ Result[C]] = self.at { c ⇒ s: State ⇒
    val call = new OnCall(c, s)
    f(call)
  }

  protected[this] final class OnCall[C <: Command] private[PlainCommandHandlerSyntax] (val command: C, val state: State) {
    def ignore: Result[C] = Xor.right(Seq.empty)
    def success(events: Event*) = Xor.right(events.toList)
    def fail[E](error: E)(implicit i: Inject[C#Error, E]) = Xor.left(i(error))
  }

  protected[this] implicit def onCallToCommand[C <: Command](call: OnCall[C]): C = call.command
}
