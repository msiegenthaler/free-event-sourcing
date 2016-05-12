package freeeventsourcing.utils

@annotation.implicitNotFound(msg = "Cannot prove that ${A} =!= ${B}.")
sealed trait =!=[A, B]
object =!= {
  implicit def apply[A, B](implicit e: A Impl B): A =!= B =
    new =!=[A, B] {}

  class Impl[A, B]
  object Impl {
    implicit def neq[A, B]: A Impl B = null
    implicit def neqAmbig1[A]: A Impl A = null
    implicit def neqAmbig2[A]: A Impl A = null
  }
}
