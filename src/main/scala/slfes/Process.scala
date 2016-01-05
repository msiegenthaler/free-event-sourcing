package slfes

import cats.data.Xor
import shapeless.ops.coproduct.Inject

case class ProcessType[A <: AggregateInterface, Id](name: String, source: A, spawn: AggregateEvt[A] ⇒ Option[Id], body: Id ⇒ ProcessBody)

sealed trait ProcessBodyAction[+A]
object ProcessBodyAction {
  case class Await[A <: AggregateInterface, R](id: A#Id, handler: AggregateEvt[A] ⇒ Option[R]) extends ProcessBodyAction[R]
  case class Command[A <: AggregateInterface, C <: Cmd](to: A#Id, command: C)(implicit cfa: A#IsCommand[C]) extends ProcessBodyAction[CommandResult[C]]
  type CommandResult[C <: Cmd] = C#Errors Xor Unit
}