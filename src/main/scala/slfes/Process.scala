package slfes

import cats.data.Xor
import shapeless.ops.coproduct.Inject

sealed trait ProcessAction[+A]
object ProcessAction {
  type CommandResult[C <: Cmd] = C#Errors Xor Unit

  case class Await[A <: Aggregate, R](id: A#Id, handler: A#Event ⇒ Option[R]) extends ProcessAction[R]
  case class Command[A <: Aggregate, C <: Cmd](to: A#Id, command: C)(implicit i: Inject[A#Command, C]) extends ProcessAction[CommandResult[C]]
}