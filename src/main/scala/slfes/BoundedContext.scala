package slfes

import shapeless.HList
import shapeless.LUBConstraint.<<:

case class BoundedContextType[AS <: HList : <<:[AggregateType]#λ, PS <: HList : <<:[ProcessType]#λ](name: String,
  aggregates: AS, processes: PS)
