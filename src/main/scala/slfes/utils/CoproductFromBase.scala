package slfes.utils

import shapeless.Generic

/** Allow for easier declaration of Generic Reprs.
  * Usage: object Commands extends CoproductFromBase(Generic[Command]) */
class CoproductFromBase[G <: Generic[_]](val generic: G) {
  type Type = generic.Repr
}