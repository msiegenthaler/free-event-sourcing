package slfes

import scala.language.higherKinds
import scala.language.implicitConversions
import cats.data.Xor
import shapeless._
import shapeless.ops.coproduct.Inject
import CoproductConstraint._


trait CommandHandler[State, Commands <: Coproduct, Events <: Coproduct] extends Poly1 {
  type Result[Cmd <: {type Error}] = Xor[Cmd#Error, Seq[Events]]
  type IsCommand[Cmd] = Inject[Commands, Cmd]
  type IsEvent[Event] = Inject[Events, Event]

  def on[Cmd <: {type Error} : IsCommand](f: Cmd ⇒ State ⇒ Result[Cmd]): Case.Aux[Cmd, State ⇒ Result[Cmd]] =
    at(c ⇒ s ⇒ f(c)(s))

  def liftEvent[Event](event: Event)(implicit inject: Inject[Events, Event]): Events = inject(event)

  // Add utility methods on the command (usage cmd.success(..))
  implicit class CommandResponse[Errors <: Coproduct](val cmd: Command[Errors]) {
    type Res = Result[cmd.type]

    def success: Res =
      Xor.right(Seq.empty)
    def success[Event: IsEvent](event: Event): Res =
      Xor.right(Seq(liftEvent(event)))
    def success(events: Seq[Events]): Res =
      Xor.right(events)
    /** Usage: success(Event1() :: Event2() :: HList) */
    def success[EventHList <: HList : <*<[Events]#λ](events: EventHList): Res =
      Xor.right(CoproductConstraint[EventHList, Events].coproductList(events))

    def fail[Error](error: Error)(implicit inject: Inject[Errors, Error]): Res =
      Xor.Left(inject(error))
  }
}
