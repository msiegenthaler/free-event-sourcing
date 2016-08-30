package freeeventsourcing.api.port

import cats.data.Xor
import freeeventsourcing.api.Aggregate

trait CommandPort {
  //TODO basically it's that, but we should do it async...
  // what's the best option?
  // - future
  // - actor like?
  // - callback?
  def on[A <: Aggregate, C <: A#Command](on: A#Id, command: C): C#Error Xor Unit
}
