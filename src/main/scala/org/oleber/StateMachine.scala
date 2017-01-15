package org.oleber

import org.oleber.State.State
import play.api.libs.json.Json

import scala.language.implicitConversions

object StateMachine {
  implicit val format = Json.format[StateMachine]
}

case class StateMachine(
                       StartAt: String,
                       States: Map[String, State],
                       Comment: Option[String] = None,
                       Version:  Option[String] = None,
                       TimeoutSeconds: Option[Long] = None
                     )
