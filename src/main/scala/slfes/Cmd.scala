package slfes

import shapeless.Coproduct

trait Cmd {
  type Errors <: Coproduct
}