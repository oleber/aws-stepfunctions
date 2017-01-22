package org.oleber

import org.oleber.state._
import play.api.libs.json._

import scala.collection.Map
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.implicitConversions

object State {

  object Implicits {

    import scala.language.implicitConversions

    implicit def toOption[T](value: T): Option[T] = Option(value)

    implicit val formatFiniteDuration = new Format[FiniteDuration] {
      override def writes(o: FiniteDuration): JsValue = JsNumber(o.toSeconds)

      override def reads(json: JsValue): JsResult[FiniteDuration] = json.validate[Long] map { seconds =>
        seconds.seconds
      }
    }
  }

  import Implicits._

  case class StateException(str: String) extends Exception

  object StateType {
    val Pass = StateType("Pass")
    val Task = StateType("Task")
    val Choice = StateType("Choice")
    val Wait = StateType("Wait")
    val Succeed = StateType("Succeed")
    val Fail = StateType("Fail")
    val Parallel = StateType("Parallel")

    implicit val format = new Format[StateType] {
      override def writes(o: StateType): JsValue =
        JsString(o.value)

      override def reads(json: JsValue): JsResult[StateType] =
        json.validate[String].map(StateType.apply)
    }
  }

  case class StateType(value: String)


  object Follow {

    object End extends Follow

    case class Next(Next: String) extends Follow

    object Next {
      val format = Json.format[Next]
    }

    implicit val format = new Format[Follow] {
      override def reads(json: JsValue): JsResult[Follow] = {
        json.validate(Next.format) orElse {
          json.validate[String].map({
            case "End" => End
          })
        }
      }

      override def writes(follow: Follow): JsValue = {
        follow match {
          case End =>
            import play.api.libs.json.Json._
            obj("End" -> true)
          case nextFollow: Next =>
            Json.toJson(nextFollow)(Next.format)
        }
      }
    }
  }

  sealed trait Follow

  trait StateVisitor[T] {

    def visit(state: PassState): T

    def visit(state: TaskState): T

    def visit(state: ChoiceState): T

    def visit(state: WaitState): T

    def visit(state: SucceedState): T

    def visit(state: FailState): T

    def visit(state: ParallelState): T
  }

  implicit val format = new Format[State] {
    override def writes(state: State): JsValue = state.accept(new StateVisitor[JsValue] {
      def reformat(stateType: StateType, value: JsValue): JsValue = {
        value match {
          case JsObject(objValue) =>
            val cleanMap: Map[String, JsValue] = objValue.get("follow") match {
              case Some(JsObject(follow)) => objValue - "follow" ++ follow
              case None => objValue
              case Some(other) => throw StateException(s"Unexpected type of follow: $other")
            }

            val newValues = cleanMap + ("Type" -> JsString(stateType.value))
            JsObject(newValues)
          case _ => throw StateException(s"Unexpected type: $value")
        }
      }

      override def visit(state: PassState): JsValue =
        reformat(StateType.Pass, Json.toJson(state)(PassState.format))

      override def visit(state: TaskState): JsValue =
        reformat(StateType.Task, Json.toJson(state)(TaskState.format))

      override def visit(state: ChoiceState): JsValue =
        reformat(StateType.Choice, Json.toJson(state)(ChoiceState.format))

      override def visit(state: WaitState): JsValue =
        reformat(StateType.Wait, Json.toJson(state)(WaitState.format))

      override def visit(state: SucceedState): JsValue =
        reformat(StateType.Succeed, Json.toJson(state)(SucceedState.format))

      override def visit(state: FailState): JsValue =
        reformat(StateType.Fail, Json.toJson(state)(FailState.format))

      override def visit(state: ParallelState): JsValue =
        reformat(StateType.Parallel, Json.toJson(state)(ParallelState.format))

    })

    override def reads(json: JsValue): JsResult[State] = {
      def jsResultFollow: JsResult[Follow] = (json \ "Next")
        .validate[String]
        .map(name => Follow.Next(name))
        .orElse(
          (json \ "End")
            .validate[Boolean]
            .map(_ => Follow.End)
        )

      val withFollowResult = jsResultFollow
        .flatMap({ follow =>
          json.validate[JsObject].map({ jsObject =>
            val cleaned = jsObject.value - "Next" - "End"
            val withFollow = cleaned + ("follow" -> Json.toJson(follow))
            JsObject(withFollow.toSeq)
          })
        })

      withFollowResult map { withFollowJSON =>
        val typ = (withFollowJSON \ "Type").as[String]
        StateType(typ) match {
          case StateType.Pass => withFollowJSON.as(PassState.format)
          case StateType.Task => withFollowJSON.as(TaskState.format)
          case StateType.Choice => withFollowJSON.as(ChoiceState.format)
          case StateType.Wait => withFollowJSON.as(WaitState.format)
          case StateType.Succeed => withFollowJSON.as(SucceedState.format)
          case StateType.Fail => withFollowJSON.as(FailState.format)
          case StateType.Parallel => withFollowJSON.as(ParallelState.format)
          case other => throw StateException(s"Unexpected Type: $typ")
        }
      }
    }
  }

  trait State {
    def accept[T](visitor: StateVisitor[T]): T

    def Comment: Option[String]
  }

  object Retrier {
    implicit val format = Json.format[Retrier]
  }

  case class Retrier(
                      ErrorEquals: List[String],
                      IntervalSeconds: Option[FiniteDuration] = None,
                      MaxAttempts: Option[Long] = None,
                      BackoffRate: Option[Float] = None
                    )

  object Catcher {
    implicit val format = Json.format[Catcher]
  }

  case class Catcher(
                      ErrorEquals: List[String],
                      Next: String,
                      ResultPath: Option[String] = None
                    )

}