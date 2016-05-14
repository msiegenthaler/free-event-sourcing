package freeeventsourcing.utils

@annotation.implicitNotFound(msg = "Cannot prove that ${A} =!= ${B}.")
sealed trait =!=[A, B]
object =!= {
  implicit def apply[A, B](implicit e: A Impl B): A =!= B =
    new =!=[A, B] {}

  sealed trait Impl[A, B]
  object Impl {
    implicit def neq[A, B]: A Impl B = new Impl[A, B] {}
    implicit def neqAmbig1[A]: A Impl A = throw new AssertionError("Cannot exist")
    implicit def neqAmbig2[A]: A Impl A = throw new AssertionError("Cannot exist")
  }
}
