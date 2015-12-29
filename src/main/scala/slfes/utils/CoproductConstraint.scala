package slfes.utils

import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HList, HNil}

import scala.annotation.implicitNotFound

/**
  * constraint that checks {{{shapeless.HList}}} elements to be of a type contained in a {{{shapeless.Coproduct}}}.
  * Usage
  *
  * Inspiration: https://stackoverflow.com/questions/32786247/shapeless-own-hlist-constraint-using-coproduct
  */
@implicitNotFound("Not all members of the HList ${L} are a members of the coproduct ${CP}.")
trait CoproductConstraint[L <: HList, CP <: Coproduct] extends Serializable {
  def coproductList(value: L): List[CP]
}

object CoproductConstraint {
  def apply[L <: HList, CP <: Coproduct](implicit cpc: CoproductConstraint[L, CP]): CoproductConstraint[L, CP] = cpc

  type <*<[CP <: Coproduct] = {
    type λ[L <: HList] = CoproductConstraint[L, CP]
  }

  implicit def hnilCP[HN <: HNil, CP <: Coproduct]: CoproductConstraint[HN, CP] =
    new CoproductConstraint[HN, CP] {
      def coproductList(value: HN) = Nil
    }
  implicit def hlistCP[H, T <: HList, CP <: Coproduct](implicit inject: coproduct.Inject[CP, H],
    cpct: CoproductConstraint[T, CP]): CoproductConstraint[H :: T, CP] =
    new CoproductConstraint[H :: T, CP] {
      def coproductList(value: H :: T) = inject(value.head) :: cpct.coproductList(value.tail)
    }
}