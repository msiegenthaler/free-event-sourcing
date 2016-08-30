package freeeventsourcing.api.adapter

import freeeventsourcing.api.ReadModel
import freeeventsourcing.api.port.PublishedEventsPort

/** Implementation of a read model. */
trait ReadModelAdapter[RM <: ReadModel] {
  def create(events: PublishedEventsPort): RM
}
