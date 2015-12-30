package slfes

import scala.language.implicitConversions
import cats.data.Xor
import shapeless.ops.coproduct.Inject
import shapeless.{HList, Poly1, Coproduct}
import slfes.utils.CoproductConstraint
import slfes.utils.CoproductConstraint._


case class CommandSyntax[State, Commands <: Coproduct, Events <: Coproduct, T <: Poly1](poly: T) {
  type Result[C <: Cmd] = Xor[C#Errors, Seq[Events]]
  type IsCommand[C] = Inject[Commands, C]

  def on[C <: Cmd : IsCommand](f: C ⇒ State ⇒ Result[C]): poly.Case.Aux[C, State ⇒ Result[C]] =
    poly.at(f)

  implicit def commandSyntaxResponse[E <: Coproduct](cmd: Cmd {type Errors = E}): CommandSyntaxResponse[E, Events] =
    CommandSyntaxResponse(cmd)
}

case class CommandSyntaxResponse[E <: Coproduct, Events <: Coproduct](val cmd: Cmd {type Errors = E}) {
  private type Res = Xor[cmd.type#Errors, Seq[Events]]
  type IsEvent[Event] = Inject[Events, Event]

  private def liftEvent[Event](event: Event)(implicit inject: Inject[Events, Event]): Events = inject(event)

  def success: Res =
    Xor.right(Seq.empty)
  def success[Event: IsEvent](event: Event): Res =
    Xor.right(Seq(liftEvent(event)))
  def success(events: Seq[Events]): Res =
    Xor.right(events)
  /** Usage: success(Event1() :: Event2() :: HList) */
  def success[EventHList <: HList : <*<[Events]#λ](events: EventHList): Res =
    Xor.right(CoproductConstraint[EventHList, Events].coproductList(events))
  //TODO add a tupeled syntax as per http://blog.scalac.io/2015/10/15/shapeless-and-futures.html

  def fail[Error](error: Error)(implicit inject: Inject[E, Error]): Res =
    Xor.Left(inject(error))
}
