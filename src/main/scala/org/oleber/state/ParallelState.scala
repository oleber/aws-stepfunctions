package org.oleber.state

import org.oleber.State.{Catcher, Follow, Retrier, State, StateVisitor}
import org.oleber.state.ParallelState.Branche
import play.api.libs.json._

import scala.collection.Map

object ParallelState {

  case class Branche(
                      StartAt: String,
                      States: Map[String, State]
                    )

  object Branche {
    implicit val formatMap = new Format[Map[String, State]] {
      override def writes(value: Map[String, State]): JsValue = {
        val pairs = value map { case (key, state) =>
          key -> Json.toJson(state)
        }
        new JsObject(pairs)
      }

      override def reads(json: JsValue): JsResult[Map[String, State]] = {
        json.validate[JsObject] map { case JsObject(pairs) =>
          pairs map { case (k, v) => k -> v.as[State] }
        }
      }
    }

    implicit val format = Json.format[Branche]
  }

  val format: Format[ParallelState] = Json.format[ParallelState]
}

case class ParallelState(
                          Branches: List[Branche],
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