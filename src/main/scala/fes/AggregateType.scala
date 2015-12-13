package fes

import scala.language.existentials
import scala.language.higherKinds

/** Describes a class of aggregates (i.e. Account or Customer). */
case class AggregateType[I, S, C[_], Ev, Er](
  name: String,
  create: I ⇒ S,
  commandToId: C[_] ⇒ I,
  commandHandler: (C[A] ⇒ CommandEffect[Ev, S, Er, A]) forSome {type A},
  eventHandler: (Ev) ⇒ (S) ⇒ S) {

  type Id = I
  type State = S
  type Command[A] = C[A]
  type Event = Ev
  type Error = Er
}