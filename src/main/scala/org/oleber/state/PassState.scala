package org.oleber.state

import org.oleber.State.{Follow, State, StateVisitor}
import play.api.libs.json.{Format, JsValue, Json}

object PassState {
  val format: Format[PassState] = Json.format[PassState]
}

case class PassState(
                      follow: Follow,
                      Result: Option[JsValue] = None,
                      InputPath: Option[String] = None,
                      OutputPath: Option[String] = None,
                      ResultPath: Option[String] = None,
                      Comment: Option[String] = None
                    ) extends State {
  override def accept[T](visitor: StateVisitor[T]): T =
    visitor.visit(this)
}
