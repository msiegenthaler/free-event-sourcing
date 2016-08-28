package freeeventsourcing.utils

/** Supertype for ADTs that are to be implemented with case classes.
 *  This prevents the 'XXX with Product with Serializable' types showing up everywhere.
 */
trait ADT extends Product with Serializable