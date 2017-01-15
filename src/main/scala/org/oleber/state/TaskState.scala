package org.oleber.state

import org.oleber.State.{Catcher, Follow, Retrier, State, StateVisitor}
import play.api.libs.json.{Format, Json}

object TaskState {
  val format: Format[TaskState] = Json.format[TaskState]
}

case class TaskState(
                      Resource: String,
                      TimeoutSeconds: Option[Int] = None, // in seconds
                      HeartbeatSeconds: Option[Int] = None, // in seconds
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
