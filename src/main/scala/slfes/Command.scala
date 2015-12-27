package slfes

import cats.data.Xor
import shapeless.Coproduct


trait Command[Err <: Coproduct] {
  type Error = Err
  type Result = Xor[Error, Unit]
}