package slfes

import shapeless.Generic

trait CoproductFromBase {
  val generic: Generic[_]
  type Type = generic.Repr
}