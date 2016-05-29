package freeeventsourcing.support

import scala.annotation.implicitNotFound
import freeeventsourcing.BoundedContext
import freeeventsourcing.eventselector.AggregateEventSelector

@implicitNotFound("${S} is not a valid EventSelector for this bounded context (${BC}).")
trait ValidSelector[BC <: BoundedContext, S]