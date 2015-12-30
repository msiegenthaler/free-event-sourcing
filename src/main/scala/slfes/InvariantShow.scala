package slfes

import scala.reflect.NameTransformer
import cats.Show

/** Renders an invariant to its pretty classname (i.e. for object 'Bad thing' extends Inv[S]). */
object InvariantShow {
  def show[S, T <: Inv[S]](invariant: T): String =
    implicitly[Show[T]].show(invariant)

  implicit def instance[State, T <: Inv[State]] = Show.show[T] { i ⇒
    val className = i.getClass.getName
    val decoded = NameTransformer.decode(className)
    val m = classPattern.matcher(decoded)
    m.find()
    m.group(1)
  }

  private val classPattern = "([^$.]+)[$]?$".r.pattern
}
