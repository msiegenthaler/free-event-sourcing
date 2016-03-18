import java.util.UUID

package object slfesakka {
  case class CommandId(private val uuid: UUID) extends AnyVal
  object CommandId {
    def apply: CommandId = CommandId(UUID.randomUUID)
  }
}
