package org.lolczak.dcg.parser.language.variable

import org.lolczak.dcg.model.{Term, Production, FeatureValue}
import org.lolczak.dcg.parser.language.ParseTree

case class VariableAssignment(private val assignments: Map[String, FeatureValue]) {

  val variables = assignments.keySet

  def add(varName: String, value: FeatureValue): Option[VariableAssignment] =
    if (assignments.contains(varName) && assignments(varName) != value) None
    else Some(VariableAssignment(assignments + (varName -> value)))

  def put(varName: String, value: FeatureValue): VariableAssignment =
    add(varName, value).getOrElse(throw new IllegalArgumentException(s"VariableAssignment contains $varName all ready"))

  def find(varName: String): Option[FeatureValue] = assignments.get(varName)

  def union(that: VariableAssignment): Option[VariableAssignment] = {
    val consistent = assignments.forall { case (varName, value) => that.find(varName).map(_ == value).getOrElse(true) }
    if (consistent) combine(that)
    else None
  }

  private def combine(that: VariableAssignment): Some[VariableAssignment] = {
    val allVariables = this.variables ++ that.variables
    val allAssignments = for {
      varName <- allVariables
      value = this.find(varName).orElse(that.find(varName)).get
    } yield (varName, value)
    Some(VariableAssignment(allAssignments.toMap))
  }

  def forEach(f: ((String, FeatureValue)) => Unit): Unit = assignments.foreach(f)

}

object VariableAssignment {

  val empty = VariableAssignment()

  def apply(elems: (String, FeatureValue)*): VariableAssignment = VariableAssignment(Map(elems: _*))

}