package slfes

import shapeless._
import slfes.utils.AnyToCoproduct

/** Describes a class of aggregates (i.e. Account or Customer). */
case class AggregateDefinition[I: Typeable, S, C <: Coproduct: AnyToCoproduct, E <: Coproduct: AnyToCoproduct](
    name: String,
    seed: I ⇒ S,
    handleCommand: C ⇒ S ⇒ CmdResult[E],
    applyEvent: E ⇒ S ⇒ S
) {
  private def outer = this

  val aggregateType = new AggregateType {
    type Interface = Aggregate.Aux[I, C, E]
    val interface: Interface = new Aggregate {
      val name = outer.name
      type Id = I
      type Command = C
      type Event = E
    }

    type Implementation = AggregateImplementation.Aux[I, S, C, E]
    val implementation: Implementation = new AggregateImplementation {
      val name: String = outer.name
      val seed: (Id) ⇒ State = outer.seed
      val handleCommand: (Command) ⇒ (State) ⇒ CmdResult[Event] = outer.handleCommand
      val applyEvent: (Event) ⇒ (State) ⇒ State = outer.applyEvent
      def AnyToCommand = implicitly
      def IdTypeable = implicitly
      def AnyToEvent = implicitly
      type Id = I
      type Command = C
      type State = S
      type Event = E
    }
  }
}

sealed trait AggregateType {
  type Interface <: Aggregate
  val interface: Interface
  type Implementation <: AggregateImplementation
  val implementation: Implementation
}
object AggregateType {
  object ToInterface extends Poly1 {
    implicit def tpe[AT <: AggregateType] = at[AT](_.interface)
  }
  object ToImplementation extends Poly1 {
    implicit def tpe[AT <: AggregateType] = at[AT](_.implementation)
  }
}

/** Public interface of an aggregate. */
sealed trait Aggregate {
  val name: String

  type Id
  type Command <: Coproduct
  type Event <: Coproduct
}
object Aggregate {
  type Aux[I, C <: Coproduct, E <: Coproduct] = Aggregate {
    type Id = I
    type Command = C
    type Event = E
  }
  type AuxIC[I, C <: Coproduct] = Aggregate {
    type Id = I
    type Command = C
  }
  type AuxIE[I, E <: Coproduct] = Aggregate {
    type Id = I
    type Event = E
  }
  type WithId[I] = Aggregate { type Id = I }
  type WithCommand[C <: Coproduct] = Aggregate { type Command = C }
}

/** Implementation of the aggregate. */
sealed trait AggregateImplementation {
  val name: String
  val seed: Id ⇒ State
  val handleCommand: Command ⇒ State ⇒ CmdResult[Event]
  val applyEvent: Event ⇒ State ⇒ State

  def AnyToCommand: AnyToCoproduct[Command]
  def AnyToEvent: AnyToCoproduct[Event]
  def IdTypeable: Typeable[Id]

  object Id {
    def unapply(a: Any): Option[Id] = IdTypeable.cast(a)
  }
  object Command {
    def unapply(a: Any): Option[Command] = AnyToCommand(a)
  }
  object Event {
    def unapply(a: Any): Option[Event] = AnyToEvent(a)
  }

  type Id
  type State
  type Command <: Coproduct
  type Event <: Coproduct
}
object AggregateImplementation {
  type Aux[I, S, C <: Coproduct, E <: Coproduct] = AggregateImplementation {
    type Id = I
    type State = S
    type Command = C
    type Event = E
  }
}