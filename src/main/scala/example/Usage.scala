package example

import scala.language.higherKinds
import example.account.{AccountProcessing, Transaction, Account}

object Usage {
  val ap = AccountProcessing.definition.interface

  def test[F[_] : ap.Exec]() = for {
    _ ← ap.execute(Account.Id(1), Account.Command.Open("Mario"))
    _ ← ap.execute(Transaction.Id(1), Transaction.Command.Confirm())
  } yield ()
}
