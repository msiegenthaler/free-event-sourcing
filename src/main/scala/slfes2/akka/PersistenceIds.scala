package slfes2.akka

import slfes.utils.StringSerializable
import slfes2.{ Aggregate, BoundedContext }
import slfesakka.CompositeName

object PersistenceIds {
  def forAggregate[A <: Aggregate](boundedContext: BoundedContext, aggregate: A)(id: A#Id)(implicit ser: StringSerializable[A#Id]): String = {
    val name = CompositeName.root / boundedContext.name / aggregate.name / ser.serializeToString(id)
    name.toString
  }
}