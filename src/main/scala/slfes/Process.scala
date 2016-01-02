package slfes

import cats.data.Xor
import shapeless.ops.coproduct.Inject

trait ProcessType[A <: Aggregate, Id] {
  val source: A
  val spawn: A#Event ⇒ Option[Id]
  val body: Id ⇒ ProcessBody
}
object ProcessType {
  def create[A <: Aggregate, Id](source: A)(spawn: A#Event ⇒ Option[Id], body: Id ⇒ ProcessBody): ProcessType[A, Id] = {
    val so = source
    val sp = spawn
    val bo = body
    new ProcessType[A, Id] {
      val source = so
      val spawn = sp
      val body = bo
    }
  }
}

sealed trait ProcessBodyAction[+A]
object ProcessBodyAction {
  case class Await[A <: Aggregate, R](id: A#Id, handler: A#Event ⇒ Option[R]) extends ProcessBodyAction[R]
  case class Command[A <: Aggregate, C <: Cmd](to: A#Id, command: C)(implicit i: Inject[A#Command, C]) extends ProcessBodyAction[CommandResult[C]]
  type CommandResult[C <: Cmd] = C#Errors Xor Unit
}