package slfes.utils

import java.util.UUID

import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil, LabelledTypeClass, LabelledTypeClassCompanion, Lazy }

import scala.language.implicitConversions
import simulacrum.typeclass

import scala.util.Try

@typeclass trait StringSerializable[A] { self ⇒
  def serializeToString(value: A): String
  def parseFromString(serialized: String): Option[A]
}

object StringSerializable { // extends LabelledTypeClassCompanion[StringSerializable] {
  implicit def stringStringSerializable = new StringSerializable[String] {
    def serializeToString(value: String) = value
    def parseFromString(serialized: String) = Some(serialized)
  }

  implicit def intStringSerializable = new StringSerializable[Int] {
    def serializeToString(value: Int) = value.toString
    def parseFromString(serialized: String) = Try(serialized.toInt).toOption
  }

  implicit def longStringSerializable = new StringSerializable[Long] {
    def serializeToString(value: Long) = value.toString
    def parseFromString(serialized: String) = Try(serialized.toLong).toOption
  }

  implicit def uuidStringSerializable = new StringSerializable[UUID] {
    def serializeToString(value: UUID) = value.toString
    def parseFromString(serialized: String) = Try(UUID.fromString(serialized)).toOption
  }

  implicit def deriveSizeOneHList[A: StringSerializable] = new StringSerializable[A :: HNil] {
    def serializeToString(value: A :: HNil) = StringSerializable[A].serializeToString(value.head)
    def parseFromString(serialized: String) = StringSerializable[A].parseFromString(serialized).map(_ :: HNil)
  }

  implicit def deriveInstance[F, G](implicit gen: Generic.Aux[F, G], cg: Lazy[StringSerializable[G]]) = new StringSerializable[F] {
    def serializeToString(value: F) = cg.value.serializeToString(gen.to(value))
    def parseFromString(serialized: String) = cg.value.parseFromString(serialized).map(gen.from _)
  }
}