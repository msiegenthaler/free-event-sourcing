package freeeventsourcing.api

trait ReadModel {
  val name: String

  //TODO more functions will be defined on the concrete type... i.e. allAccounts()
}

//TODO add them to bounded context