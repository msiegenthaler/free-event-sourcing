package slfes.utils

import shapeless.{ :+:, CNil, Coproduct, Inl, Inr, Typeable }

/** Converts Any into Option[C <: Coproduct]. Used to check at runtime if a value matches the coproduct type. */
sealed trait AnyToCoproduct[C <: Coproduct] {
  def apply(value: Any): Option[C]
}
object AnyToCoproduct {
  def apply[C <: Coproduct](value: Any)(implicit x: AnyToCoproduct[C]): Option[C] =
    x.apply(value)

  implicit def hd[H, T <: Coproduct](implicit head: Typeable[H], tail: AnyToCoproduct[T]) = new AnyToCoproduct[H :+: T] {
    def apply(v: Any) = {
      head.cast(v)
        .map(v ⇒ Inl[H, T](v))
        .orElse(tail(v).map(v ⇒ Inr(v)))
    }
  }
  implicit def tl: AnyToCoproduct[CNil] = new AnyToCoproduct[CNil] {
    def apply(v: Any) = None
  }
}