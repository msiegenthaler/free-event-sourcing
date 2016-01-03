package slfes

import cats.data.Xor
import shapeless.ops.coproduct.Inject

case class ProcessType[A <: Aggregate, Id](name: String, source: A, spawn: AggregateEvt[A] ⇒ Option[Id], body: Id ⇒ ProcessBody)

sealed trait ProcessBodyAction[+A]
object ProcessBodyAction {
  case class Await[A <: Aggregate, R](id: A#Id, handler: AggregateEvt[A] ⇒ Option[R]) extends ProcessBodyAction[R]
  case class Command[A <: Aggregate, C <: Cmd](to: A#Id, command: C)(implicit i: Inject[A#Command, C]) extends ProcessBodyAction[CommandResult[C]]
  type CommandResult[C <: Cmd] = C#Errors Xor Unit
}