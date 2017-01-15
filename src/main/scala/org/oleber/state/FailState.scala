package org.oleber.state

import org.oleber.State.{State, StateVisitor}
import play.api.libs.json.{Format, Json}

object FailState {
  val format: Format[FailState] = Json.format[FailState]
}

case class FailState(
                      Error: String,
                      Cause: String,
                      Comment: Option[String] = None
                    ) extends State {
  override def accept[T](visitor: StateVisitor[T]): T =
    visitor.visit(this)
}