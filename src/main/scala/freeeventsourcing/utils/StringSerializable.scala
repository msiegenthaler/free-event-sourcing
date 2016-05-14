package freeeventsourcing.utils

import scala.language.implicitConversions
import java.util.UUID
import scala.util.Try
import shapeless._
import simulacrum.typeclass

@typeclass trait StringSerializable[A] { self ⇒
  def serializeToString(value: A): String
  def parseFromString(serialized: String): Option[A]
}

object StringSerializable {
  implicit def string = new StringSerializable[String] {
    def serializeToString(value: String) = value
    def parseFromString(serialized: String) = Some(serialized)
  }

  implicit def int = new StringSerializable[Int] {
    def serializeToString(value: Int) = value.toString
    def parseFromString(serialized: String) = Try(serialized.toInt).toOption
  }

  implicit def long = new StringSerializable[Long] {
    def serializeToString(value: Long) = value.toString
    def parseFromString(serialized: String) = Try(serialized.toLong).toOption
  }

  implicit def uuid = new StringSerializable[UUID] {
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