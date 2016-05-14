package freeeventsourcing.utils

import org.scalatest.{ FlatSpec, Matchers }
import CompositeName.root

class CompositeNameTests extends FlatSpec with Matchers {

  "CompositeName " should " be composable from multiple strings" in {
    root / "hello" / "world"
  }

  "CompositeName " should " be composable from two composite names" in {
    val a = root / "hi" / "there"
    val b = root / "mario" / "siegenthaler"
    val c1 = a / b
    val c2 = root / "hi" / "there" / "mario" / "siegenthaler"
    c1 shouldBe c2
  }

  "CompositeName " should " have a simple constructor" in {
    CompositeName("hi") shouldBe (root / "hi")
    CompositeName("hi", "mario") shouldBe (root / "hi" / "mario")
  }

  "CompositeName.parent " should " return the parent composite name" in {
    val a = root / "hi" / "there"
    a.parent shouldBe (root / "hi")
  }

  "CompositeName.parent " should " return root on the topmost name" in {
    val a = root / "hi"
    a.parent shouldBe root
  }

  "CompositeName.parent " should " return root for root" in {
    val a = root
    a.parent shouldBe root
  }

  "CompositeName " should " serialize to a string" in {
    val a = root / "hi" / "there"
    a.serialize shouldBe "/hi/there"
  }

  "CompositeName " should " serialize to a clean string if the name contains the separator" in {
    val a = root / "hi" / "there/mario"
    a.serialize shouldBe "/hi/there%2Fmario"
  }

  "CompositeName " should " serialize to an url encoded string" in {
    val a = root / "hi" / "there"
    a.urlEncoded shouldBe "hi+there"
  }

  "CompositeName " should " serialize to a clean url encoded string if the name contains the separator" in {
    val a = root / "hi" / "th+ere/mario"
    a.urlEncoded shouldBe "hi+th%2Bere%2Fmario"
  }

  "CompositeName " should " parse from a string" in {
    val a = CompositeName.parse("/hi/there")
    a shouldBe Some(root / "hi" / "there")
  }

  "CompositeName " should " parse from a string even if it contains the separator char" in {
    val a = CompositeName.parse("/hi/there%2Fmario")
    a shouldBe Some(root / "hi" / "there/mario")
  }

  "CompositeName " should " not parse from a malformed string" in {
    val a = CompositeName.parse("hi/there")
    a shouldBe None
  }

  val testData = List(
    root,
    root / "a",
    root / "頦",
    root / "12!@",
    root / "/",
    root / "/a%$頦",
    root / "a%$頦/",
    root / "/a%$頦/",
    root / "/a%$頦/" / "b",
    root / "/a%$頦/" / "/b",
    root / "/a%$頦/" / "/b頦/",
    root / "/a%$頦/" / "/b頦/" / "☭",
    (1 to 1000).map(_.toChar).grouped(3).map(_.mkString("")).foldLeft(root)(_ / _)
  )

  "CompositeName " should " roundtrip the serialization" in testData.foreach { name ⇒
    val ser = name.serialize
    val par = CompositeName.parse(ser)
    par shouldBe Some(name)
    par.map(_.serialize) shouldBe Some(ser)
  }

  "CompositeName.toString " should " equal the serialization" in testData.foreach { name ⇒
    name.toString shouldBe name.serialize
  }

  "CompositeName " should " have an unapplySeq from CompositeName" in testData.foreach { name ⇒
    name match {
      case CompositeName(n @ _*) ⇒
        n should contain theSameElementsInOrderAs name.parts
    }
  }

  "CompositeName " should " have an unapplySeq from String" in testData.foreach { name ⇒
    name.serialize match {
      case CompositeName(n @ _*) ⇒
        n should contain theSameElementsInOrderAs name.parts
    }
  }

}
