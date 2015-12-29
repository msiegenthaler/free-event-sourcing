package slfes

import cats.data.Xor
import shapeless.ops.coproduct.Inject
import shapeless.{HList, Poly1, Coproduct}
import slfes.utils.CoproductConstraint
import slfes.utils.CoproductConstraint._

case class CommandSyntax[State, Commands <: Coproduct, Events <: Coproduct, T <: Poly1](poly: T) {
  type Result[C <: Cmd] = Xor[C#Errors, Seq[Events]]
  type IsCommand[C] = Inject[Commands, C]
  type IsEvent[Event] = Inject[Events, Event]

  def on[C <: Cmd : IsCommand](f: C ⇒ State ⇒ Result[C]): poly.Case.Aux[C, State ⇒ Result[C]] =
    poly.at(c ⇒ s ⇒ f(c)(s))

  def liftEvent[Event](event: Event)(implicit inject: Inject[Events, Event]): Events = inject(event)

  // Add utility methods on the command (usage cmd.success(..))
  implicit class CommandResponse[E <: Coproduct](val cmd: Cmd {type Errors = E}) {
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

    def fail[Error](error: Error)(implicit inject: Inject[E, Error]): Res =
      Xor.Left(inject(error))
  }
}
