package slfes2.syntax

trait MatchEventApplicator[Event, State] extends Function2[Event, State, State]