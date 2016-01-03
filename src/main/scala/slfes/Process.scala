package slfes

import cats.data.Xor
import shapeless.ops.coproduct.Inject

trait ProcessType[A <: Aggregate, Id] {
  val name: String
  val source: A
  val spawn: AggregateEvt[A] ⇒ Option[Id]
  val body: Id ⇒ ProcessBody
}
object ProcessType {
  def apply[A <: Aggregate, Id](name: String, source: A)(spawn: AggregateEvt[A] ⇒ Option[Id], body: Id ⇒ ProcessBody): ProcessType[A, Id] = {
    val na = name
    val so = source
    val sp = spawn
    val bo = body
    new ProcessType[A, Id] {
      val name = na
      val source = so
      val spawn = sp
      val body = bo
    }
  }
}

sealed trait ProcessBodyAction[+A]
object ProcessBodyAction {
  case class Await[A <: Aggregate, R](id: A#Id, handler: AggregateEvt[A] ⇒ Option[R]) extends ProcessBodyAction[R]
  case class Command[A <: Aggregate, C <: Cmd](to: A#Id, command: C)(implicit i: Inject[A#Command, C]) extends ProcessBodyAction[CommandResult[C]]
  type CommandResult[C <: Cmd] = C#Errors Xor Unit
}