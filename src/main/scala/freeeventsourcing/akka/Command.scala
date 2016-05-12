package freeeventsourcing.akka

import java.util.UUID

case class CommandId(private val uuid: UUID) extends AnyVal
object CommandId {
  def generate = CommandId(UUID.randomUUID)
}

case class CommandExecuted(id: CommandId)
case class CommandFailed(id: CommandId, error: Any)
case class CommandInvalid(id: CommandId, reason: String)