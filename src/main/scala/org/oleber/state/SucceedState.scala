package org.oleber.state

import org.oleber.State.{State, StateVisitor}
import play.api.libs.json.{Format, Json}

object SucceedState {
  val format: Format[SucceedState] = Json.format[SucceedState]
}

case class SucceedState(
                         InputPath: Option[String] = None,
                         OutputPath: Option[String] = None,
                         Comment: Option[String] = None
                       ) extends State {
  override def accept[T](visitor: StateVisitor[T]): T =
    visitor.visit(this)
}
