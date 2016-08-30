package freeeventsourcing.api

//TODO when is something a read model and when is it an normal adapter using the published events port?
//TODO actually the interface might be the same (it may actually just get a reference to the publishedeventsport
// when implemented)
//TODO special is that its usable for all users of the domain model, so it should be reusable.. else adapters would
// have dependencies among each other which is not desirable

trait ReadModel {
  //TODO do we need that? basically that's the only thing that makes us require this trait.. might be better to
  // get rid of it
  val name: String

  //TODO more functions will be defined on the concrete type... i.e. allAccounts()
}