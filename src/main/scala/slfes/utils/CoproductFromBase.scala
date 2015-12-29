package slfes.utils

import shapeless.Generic

trait CoproductFromBase {
  val generic: Generic[_]
  type Type = generic.Repr
}