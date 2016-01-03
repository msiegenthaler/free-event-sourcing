package slfes

import shapeless.{LUBConstraint, HList}

case class BoundedContextType[AS <: HList, PS <: HList](name: String, aggregates: AS, processes: PS)
  (implicit evA: LUBConstraint[AS, AggregateType[_, _, _, _]],
    evP: LUBConstraint[PS, ProcessType[_, _]])
