package org.oleber.state

import org.oleber.State.{Catcher, Follow, Retrier, State, StateVisitor}
import play.api.libs.json.{Format, Json}

import scala.concurrent.duration._

object TaskState {
  import org.oleber.State.Implicits._
  val format: Format[TaskState] = Json.format[TaskState]
}

case class TaskState(
                      Resource: String,
                      TimeoutSeconds: Option[FiniteDuration] = None,
                      HeartbeatSeconds: Option[FiniteDuration] = None,
                      follow: Follow,
                      InputPath: Option[String] = None,
                      OutputPath: Option[String] = None,
                      ResultPath: Option[String] = None,
                      Retry: Option[List[Retrier]] = None,
                      Catch: Option[List[Catcher]] = None,
                      Comment: Option[String] = None
                    ) extends State {
  override def accept[T](visitor: StateVisitor[T]): T =
    visitor.visit(this)
}
