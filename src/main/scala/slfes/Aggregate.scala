package slfes

import shapeless._

/** Describes a class of aggregates (i.e. Account or Customer). */
case class AggregateDefinition[I, S, C <: Coproduct, E <: Coproduct](
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
  object ToAggregateInterface extends Poly1 {
    implicit def tpe[AT <: AggregateType] = at[AT](_.interface)
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