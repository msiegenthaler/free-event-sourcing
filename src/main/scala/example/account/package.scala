package example

import scala.language.existentials

package object account {
  type Amount = Int

  object Aggregates {
    implicit val account = AccountAggregate.description.aggregate
    implicit val transaction = TransactionAggregate.description.aggregate
  }
}