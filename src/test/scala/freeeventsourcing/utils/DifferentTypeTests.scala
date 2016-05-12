package freeeventsourcing.utils

import org.scalatest.{ FlatSpec, Matchers }

class DifferentTypeTests extends FlatSpec with Matchers {
  "DifferentType " should " allow String =!= Int to compile" in {
    "=!=[String, Int]" should compile
  }

  "DifferentType " should " not allow Int =!= Int to compile" in {
    "=!=[Int, Int]" shouldNot compile
  }

  "DifferentType " should " allow Seq[Int] =!= List[Int] to compile" in {
    "=!=[Seq[Int], List[Int]]" should compile
    "=!=[List[Int], Seq[Int]]" should compile
  }

  "DifferentType " should " not allow Seq[Int] =!= Seq[Int] to compile" in {
    "=!=[Seq[Int], Seq[Int]]" shouldNot compile
  }

  "DifferentType " should " not allow Seq[Int] =!= Seq[String] to compile" in {
    "=!=[Seq[Int], Seq[String]]" should compile
    "=!=[Seq[String], Seq[Int]]" should compile
  }
}
