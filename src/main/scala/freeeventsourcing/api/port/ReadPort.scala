package freeeventsourcing.api.port

import freeeventsourcing.api.ReadModel

/** Allows to use a read model. */
// all read models are public because we don't use them in the domain model
trait ReadPort {
  //TODO we'll need to typecheck if it's actually a valid read model for the BC..
  def on[RM <: ReadModel]: RM

  //basic idea is that we can just use the read models interface.. probably not good when we abstract that away,
  // since it's only used here anyway
}
