package slfes

import cats.data.Xor
import shapeless.Coproduct


trait Command[Errors <: Coproduct] {
  type Result = Xor[Errors, Unit]
}