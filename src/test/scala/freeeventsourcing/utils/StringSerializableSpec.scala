package freeeventsourcing.utils

import java.util.UUID
import StringSerializable.ops._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import shapeless.{ HNil, :: }

case class Example(a: Int)

class StringSerializableSpec extends PropSpec with PropertyChecks with Matchers {

  property("has identity instance for string") {
    forAll { (a: String) ⇒
      a.serializeToString shouldBe (a)
    }
  }

  def roundtrip[A: StringSerializable](a: A) = {
    val s = a.serializeToString
    val a2 = StringSerializable[A].parseFromString(s)
    a2 shouldBe Some(a)
  }

  property("has instance for Int that parses back") {
    forAll(roundtrip[Int] _)
  }
  property("has instance for Long that parses back") {
    forAll(roundtrip[Long] _)
  }

  implicit def uuid = Arbitrary(Gen.uuid)

  property("has instance for UUID that parses back") {
    forAll(roundtrip[UUID] _)
  }

  def stringHlistsSize1 = for {
    s ← Gen.alphaStr
  } yield s :: HNil

  property("has instance for (String :: HNil) that parses back") {
    forAll(stringHlistsSize1)(roundtrip[String :: HNil] _)
  }

  def example = for {
    a ← Arbitrary.arbitrary[Int]
  } yield Example(a)

  property("has derived instance for single argument case classes") {
    forAll(example)(e ⇒ roundtrip[Example](e)(StringSerializable.deriveInstance))
  }

}
