package freeeventsourcing.syntax

import scala.language.implicitConversions
import freeeventsourcing.api.domainmodel.{ BoundedContext, EventSelector, EventWithMetadata, ProcessDefinition }
import freeeventsourcing.api.domainmodel.EventSelector._
import freeeventsourcing.api.domainmodel.eventselector.ValidSelector

/** Helps defining a process definition with an easier syntax.
 *  <code>
 *   object MyProcess extends ProcessHelper(MyBoundedContext, "MyProcess")(mySelector) {
 *     protected[this] case class Instance(event: Event) extends ProcessInstance {
 *       import syntax._
 *       def process = (...)
 *     }
 *   }
 *  </code>
 */
abstract class ProcessHelper[BC <: BoundedContext, S <: WithEventType: EventSelector: ValidSelector[BC, ?]](boundedContext: BC, name: String)(selector: S) {
  val definition: ProcessDefinition[BC] = ProcessDefinition(boundedContext, name)(selector)(e ⇒ Instance(e).process)

  protected type Event = EventWithMetadata[S#Event]
  protected type Instance <: ProcessInstance
  protected[this] val Instance: Event ⇒ Instance

  protected[this] abstract class ProcessInstance {
    protected[this] val syntax = ProcessSyntax(boundedContext)
    def process: ProcessDefinition.ProcessMonad[BC, _]
  }
}
object ProcessHelper {
  implicit def helperToDefinition[BC <: BoundedContext](h: ProcessHelper[BC, _]): ProcessDefinition[BC] =
    h.definition
  implicit def helperListToDefinitionList[BC <: BoundedContext](hs: List[ProcessHelper[BC, _]]): List[ProcessDefinition[BC]] =
    hs.map(helperToDefinition)
}