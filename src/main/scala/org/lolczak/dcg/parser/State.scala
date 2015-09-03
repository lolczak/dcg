package org.lolczak.dcg.parser

case class State(edges: Set[Edge]) {

  private val prefixes: Map[String, Set[Active]] = edges.filter(_.isActive).map { case a: Active => a }.groupBy(_.remaining.head.name).withDefaultValue(Set.empty)

  def findActiveStartingWith(symbol: String): Set[Active] = prefixes(symbol)

}
