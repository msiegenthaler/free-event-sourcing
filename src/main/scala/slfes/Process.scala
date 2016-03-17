package slfes

case class ProcessDefinition[A <: Aggregate, I](name: String, source: A, spawn: AggregateEvt[A] ⇒ Option[I],
    body: I ⇒ ProcessBody) {
  private def outer = this
  val processType = new ProcessType {
    type Implementation = ProcessImplementation.Aux[A, I]
    val implementation = new ProcessImplementation {
      type Source = A
      type Id = I
      val name = outer.name
      val source = outer.source
      val spawn = outer.spawn
      val body = outer.body
    }
  }
}

sealed trait ProcessType {
  type Implementation <: ProcessImplementation
  val implementation: Implementation
}

sealed trait ProcessImplementation {
  val name: String
  val source: Source
  val spawn: AggregateEvt[Source] ⇒ Option[Id]
  val body: Id ⇒ ProcessBody

  type Source <: Aggregate
  type Id
}
object ProcessImplementation {
  type Aux[S <: Aggregate, I] = ProcessImplementation {
    type Source = S
    type Id = I
  }
}

sealed trait ProcessBodyAction[+A]
object ProcessBodyAction {
  case class Await[A <: Aggregate, R](id: A#Id, handler: AggregateEvt[A] ⇒ Option[R])
    extends ProcessBodyAction[R]
  case class Command[A <: Aggregate, C <: Cmd: CommandFor[A]#λ](to: A#Id, command: C)
    extends ProcessBodyAction[Unit]
}