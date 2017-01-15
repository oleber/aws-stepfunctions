package org.oleber

import org.oleber.NamedState.StateName
import org.oleber.State.State
import play.api.libs.json.{Format, JsResult, JsValue, Json}

import scala.language.implicitConversions

object StateMachine {
  implicit val format = new Format[StateMachine] {
    override def writes(stateMachine: StateMachine): JsValue =
      Json.toJson[RawStateMachine](stateMachine)

    override def reads(json: JsValue): JsResult[StateMachine] =
      json.validate[RawStateMachine] map {rawStateMachine => rawStateMachine}
  }

  implicit def fromRawStateMachine(rawStateMachine: RawStateMachine): StateMachine = {
    StateMachine(
      StartAt = rawStateMachine.StartAt,
      States = rawStateMachine.States.toList map { case (name, state) => NamedState[State](name, state) },
      Comment = rawStateMachine.Comment,
      Version = rawStateMachine.Version,
      TimeoutSeconds = rawStateMachine.TimeoutSeconds
    )
  }

}

case class StateMachine(
                         StartAt: StateName,
                         States: List[NamedState[State]],
                         Comment: Option[String] = None,
                         Version:  Option[String] = None,
                         TimeoutSeconds: Option[Long] = None
                       )
