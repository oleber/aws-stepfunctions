package org.oleber

import org.oleber.State.State
import play.api.libs.json.Json

import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

object StateMachine {
  import org.oleber.State.Implicits._
  implicit val format = Json.format[StateMachine]
}

case class StateMachine(
                       StartAt: String,
                       States: Map[String, State],
                       Comment: Option[String] = None,
                       Version:  Option[String] = None,
                       TimeoutSeconds: Option[FiniteDuration] = None
                     )
