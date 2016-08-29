package freeeventsourcing.api

import shapeless.HList

//TODO should this actually be named domain model? the BC might be bigger and include the adapters?
//TODO 'domainmodel' might also be a better package name than 'api', then we could move port to top level
//TODO we might also keep api and nest inside it the domainmodel and the port package.. because akka will then still fit in
trait BoundedContext {
  self ⇒
  val name: String

  /** subtype of Aggregate */
  type Aggregates <: HList
  val aggregates: Aggregates

  /** subtype of ReadModel */
  type ReadModels <: HList
  val readModels: ReadModels

  /** subtype of EventGroup */
  type EventGroups <: HList
  val eventGroups: EventGroups

  /** Convenience definition for object (xx.BoundedContext instead of xx.type). */
  type BoundedContext = self.type
}