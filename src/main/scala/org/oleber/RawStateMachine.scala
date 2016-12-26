package org.oleber

import org.oleber.NamedState.{NamedStateException, StateName}
import org.oleber.State.State
import play.api.libs.json.Json

import scala.concurrent.Promise
import scala.util.{Failure, Success}

object RawStateMachine {
  implicit val format = Json.format[RawStateMachine]

  def stateFor(namedState: NamedState[State]): State = namedState.promise.future.value match {
    case Some(Success(value)) => value
    case Some(Failure(exception)) => throw NamedStateException("Failed to evaluate State", exception)
    case None => throw NamedStateException(s"State not evaluated for ${namedState.name.name}")
  }

  implicit def fromStateMachine(stateMachine: StateMachine): RawStateMachine = {

    RawStateMachine(
      StartAt = stateMachine.StartAt.name,
      States = stateMachine.States.map(namedState => namedState.name.name -> stateFor(namedState)).toMap,
      Comment = stateMachine.Comment,
      Version = stateMachine.Version,
      TimeoutSeconds = stateMachine.TimeoutSeconds
    )
  }
}

case class RawStateMachine(
                       StartAt: StateName,
                       States: Map[String, State],
                       Comment: Option[String] = None,
                       Version:  Option[String] = None,
                       TimeoutSeconds: Option[Long] = None
                     )
