package org.oleber

import java.time.ZonedDateTime

import org.oleber.NamedState.StateName
import org.oleber.State.ChoiceState.{Choice, TopChoice}
import play.api.libs.json._

import scala.collection.Map
import scala.language.implicitConversions
import scala.util.{Failure, Try}

object State {

  object Implicits {

    import scala.language.implicitConversions

    implicit def toOption[T](value: T): Option[T] = Option(value)
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

    object EndFollow extends Follow

    case class NextFollow(Next: StateName) extends Follow

    object NextFollow {
      val format = Json.format[NextFollow]
    }

    def End: Follow = EndFollow

    def Next(namedState: StateName): Follow = NextFollow(namedState)

    implicit val format = new Format[Follow] {
      override def reads(json: JsValue): JsResult[Follow] = {
        json.validate(NextFollow.format) orElse {
          json.validate[String].map({
            case "End" => End
          })
        }
      }

      override def writes(follow: Follow): JsValue = {
        follow match {
          case EndFollow =>
            JsString("End")
          case nextFollow: NextFollow =>
            Json.toJson(nextFollow)(NextFollow.format)
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
        .map(name => Follow.Next(StateName(name)))
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

  sealed trait State {
    def accept[T](visitor: StateVisitor[T]): T

    def Comment: Option[String]
  }

  object Retrier {
    implicit val format = Json.format[Retrier]
  }

  case class Retrier(
                      ErrorEquals: List[String],
                      IntervalSeconds: Option[Long] = None,
                      MaxAttempts: Option[Long] = None,
                      BackoffRate: Option[Float] = None
                    )

  object Catcher {
    implicit val format = Json.format[Catcher]
  }

  case class Catcher(
                      ErrorEquals: List[String],
                      Next: StateName,
                      ResultPath: Option[String] = None
                    )


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

  object ChoiceState {

    trait ChoiceVisitor[T] {
      def visit(value: Not): T

      def visit(value: And): T

      def visit(value: TimestampGreaterThanEquals): T

      def visit(value: TimestampEquals): T

      def visit(value: TimestampLessThanEquals): T

      def visit(value: TimestampLessThan): T

      def visit(value: NumericGreaterThanEquals): T

      def visit(value: Or): T

      def visit(value: TimestampGreaterThan): T

      def visit(value: NumericLessThanEquals): T

      def visit(value: StringGreaterThanEquals): T

      def visit(value: StringLessThanEquals): T

      def visit(value: BooleanEquals): T

      def visit(value: NumericGreaterThan): T

      def visit(value: NumericLessThan): T

      def visit(value: NumericEquals): T

      def visit(value: StringGreaterThan): T

      def visit(value: StringLessThan): T

      def visit(value: StringEquals): T
    }

    sealed trait Choice {
      def accept[T](visitor: ChoiceVisitor[T]): T
    }

    object Choice {

      implicit val format = new Format[Choice] {
        override def reads(json: JsValue): JsResult[Choice] = {
          val objects: List[OFormat[_ <: Choice]] = List(
            BooleanEquals.format,
            NumericEquals.format,
            NumericGreaterThanEquals.format,
            NumericGreaterThan.format,
            NumericLessThanEquals.format,
            NumericLessThan.format,
            StringEquals.format,
            StringGreaterThanEquals.format,
            StringGreaterThan.format,
            StringLessThanEquals.format,
            StringLessThan.format,
            TimestampEquals.format,
            TimestampGreaterThanEquals.format,
            TimestampGreaterThan.format,
            TimestampLessThanEquals.format,
            TimestampLessThan.format,
            And.format,
            Or.format,
            Not.format
          )

          def parse(f: OFormat[_ <: Choice]): JsResult[Choice] =
            json.validate(f)


          objects.foldLeft(parse(objects.head)) {
            case (jsError, f) =>
              jsError.orElse(parse(f))
          }
        }

        override def writes(choice: Choice): JsValue = {
          choice.accept(new ChoiceVisitor[JsValue] {
            override def visit(value: Not): JsValue = Json.toJson(value)(Not.format)

            override def visit(value: NumericGreaterThan): JsValue = Json.toJson(value)(NumericGreaterThan.format)

            override def visit(value: NumericLessThan): JsValue = Json.toJson(value)(NumericLessThan.format)

            override def visit(value: NumericEquals): JsValue = Json.toJson(value)(NumericEquals.format)

            override def visit(value: StringGreaterThan): JsValue = Json.toJson(value)(StringGreaterThan.format)

            override def visit(value: StringLessThan): JsValue = Json.toJson(value)(StringLessThan.format)

            override def visit(value: NumericGreaterThanEquals): JsValue = Json.toJson(value)(NumericGreaterThanEquals.format)

            override def visit(value: TimestampLessThan): JsValue = Json.toJson(value)(TimestampLessThan.format)

            override def visit(value: TimestampLessThanEquals): JsValue = Json.toJson(value)(TimestampLessThanEquals.format)

            override def visit(value: TimestampEquals): JsValue = Json.toJson(value)(TimestampEquals.format)

            override def visit(value: TimestampGreaterThanEquals): JsValue = Json.toJson(value)(TimestampGreaterThanEquals.format)

            override def visit(value: And): JsValue = Json.toJson(value)(And.format)

            override def visit(value: BooleanEquals): JsValue = Json.toJson(value)(BooleanEquals.format)

            override def visit(value: StringLessThanEquals): JsValue = Json.toJson(value)(StringLessThanEquals.format)

            override def visit(value: StringGreaterThanEquals): JsValue = Json.toJson(value)(StringGreaterThanEquals.format)

            override def visit(value: NumericLessThanEquals): JsValue = Json.toJson(value)(NumericLessThanEquals.format)

            override def visit(value: TimestampGreaterThan): JsValue = Json.toJson(value)(TimestampGreaterThan.format)

            override def visit(value: Or): JsValue = Json.toJson(value)(Or.format)

            override def visit(value: StringEquals): JsValue = Json.toJson(value)(StringEquals.format)
          })
        }
      }
    }

    case class Numeric(either: Either[Long, Double])

    object Numeric {
      implicit def apply(value: Long): Numeric = Numeric(Left(value))

      implicit def apply(value: Double): Numeric = Numeric(Right(value))

      implicit def apply(value: Int): Numeric = Numeric(Left(value.toLong))

      implicit val format = new Format[Numeric] {
        override def reads(json: JsValue): JsResult[Numeric] = json
          .validate[Long]
          .map(Numeric.apply)
          .orElse(
            json
              .validate[Double]
              .map(Numeric.apply)
          )

        override def writes(o: Numeric): JsValue = o.either match {
          case Left(value) => Json.toJson(value)
          case Right(value) => Json.toJson(value)
        }
      }
    }

    case class StringEquals(Variable: String, StringEquals: String) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object StringEquals {
      val format = Json.format[StringEquals]
    }

    case class StringLessThan(Variable: String, StringLessThan: String) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object StringLessThan {
      val format = Json.format[StringLessThan]
    }

    case class StringGreaterThan(Variable: String, StringGreaterThan: String) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object StringGreaterThan {
      val format = Json.format[StringGreaterThan]
    }

    case class StringLessThanEquals(Variable: String, StringLessThanEquals: String) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object StringLessThanEquals {
      val format = Json.format[StringLessThanEquals]
    }

    case class StringGreaterThanEquals(Variable: String, StringGreaterThanEquals: String) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object StringGreaterThanEquals {
      val format = Json.format[StringGreaterThanEquals]
    }

    case class NumericEquals(Variable: String, NumericEquals: Numeric) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object NumericEquals {
      val format = Json.format[NumericEquals]
    }

    case class NumericLessThan(Variable: String, NumericLessThan: Numeric) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object NumericLessThan {
      val format = Json.format[NumericLessThan]
    }

    case class NumericGreaterThan(Variable: String, NumericGreaterThan: Numeric) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object NumericGreaterThan {
      val format = Json.format[NumericGreaterThan]
    }

    case class NumericLessThanEquals(Variable: String, NumericLessThanEquals: Numeric) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object NumericLessThanEquals {
      val format = Json.format[NumericLessThanEquals]
    }

    case class NumericGreaterThanEquals(Variable: String, NumericGreaterThanEquals: Numeric) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object NumericGreaterThanEquals {
      val format = Json.format[NumericGreaterThanEquals]
    }

    case class BooleanEquals(Variable: String, BooleanEquals: Boolean) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object BooleanEquals {
      val format = Json.format[BooleanEquals]
    }

    case class TimestampEquals(Variable: String, TimestampEquals: ZonedDateTime) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object TimestampEquals {
      val format = Json.format[TimestampEquals]
    }

    case class TimestampLessThan(Variable: String, TimestampLessThan: ZonedDateTime) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object TimestampLessThan {
      val format = Json.format[TimestampLessThan]
    }

    case class TimestampGreaterThan(Variable: String, TimestampGreaterThan: ZonedDateTime) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object TimestampGreaterThan {
      val format = Json.format[TimestampGreaterThan]
    }

    case class TimestampLessThanEquals(Variable: String, TimestampLessThanEquals: ZonedDateTime) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object TimestampLessThanEquals {
      val format = Json.format[TimestampLessThanEquals]
    }

    case class TimestampGreaterThanEquals(Variable: String, TimestampGreaterThanEquals: ZonedDateTime) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object TimestampGreaterThanEquals {
      val format = Json.format[TimestampGreaterThanEquals]
    }

    case class And(And: List[Choice]) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object And {
      val format: OFormat[And] = Json.format[And]
    }

    case class Or(Or: List[Choice]) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object Or {
      val format: OFormat[Or] = Json.format[Or]
    }

    case class Not(Not: Choice) extends Choice {
      override def accept[T](visitor: ChoiceVisitor[T]): T = visitor.visit(this)
    }

    object Not {
      val format: OFormat[Not] = Json.format[Not]
    }


    case class TopChoice(choice: Choice, Next: StateName)

    object TopChoice {
      implicit val format = new Format[TopChoice] {
        override def reads(json: JsValue): JsResult[TopChoice] = for {
          next <- (json \ "Next").validate[String]
          choice <- json.validate[Choice]
        } yield TopChoice(choice, next)


        override def writes(o: TopChoice): JsValue = {
          Json.toJson(o.choice) match {
            case JsObject(pairs) =>
              val nextJson = "Next" -> Json.toJson(o.Next)
              val newPairs = pairs + nextJson
              JsObject(newPairs)
            case other =>
              throw StateException(s"Json not expected as Choice representation: $other")
          }
        }
      }
    }

    val format: Format[ChoiceState] = Json.format[ChoiceState]
  }

  case class ChoiceState(
                          Choices: List[TopChoice],
                          Default: Option[StateName],
                          InputPath: Option[String] = None,
                          OutputPath: Option[String] = None,
                          Comment: Option[String] = None
                        ) extends State {
    override def accept[T](visitor: StateVisitor[T]): T =
      visitor.visit(this)
  }

  object WaitState {
    def Seconds(
                 Seconds: Long,
                 follow: Follow,
                 InputPath: Option[String] = None,
                 OutputPath: Option[String] = None,
                 Comment: Option[String] = None
               ) = WaitState(
      Seconds = Seconds,
      follow = follow,
      InputPath = InputPath,
      OutputPath = OutputPath,
      Comment = Comment
    )

    def SecondsPath(
                     SecondsPath: String,
                     follow: Follow,
                     InputPath: Option[String] = None,
                     OutputPath: Option[String] = None,
                     Comment: Option[String] = None
                   ) = WaitState(
      SecondsPath = SecondsPath,
      follow = follow,
      InputPath = InputPath,
      OutputPath = OutputPath,
      Comment = Comment
    )

    def Timestamp(
                   Timestamp: ZonedDateTime,
                   follow: Follow,
                   InputPath: Option[String] = None,
                   OutputPath: Option[String] = None,
                   Comment: Option[String] = None
                 ) = WaitState(
      Timestamp = Timestamp,
      follow = follow,
      InputPath = InputPath,
      OutputPath = OutputPath,
      Comment = Comment
    )

    def TimestampPath(
                       TimestampPath: String,
                       follow: Follow,
                       InputPath: Option[String] = None,
                       OutputPath: Option[String] = None,
                       Comment: Option[String] = None
                     ) = WaitState(
      TimestampPath = TimestampPath,
      follow = follow,
      InputPath = InputPath,
      OutputPath = OutputPath,
      Comment = Comment
    )

    val format: Format[WaitState] = Json.format[WaitState]
  }

  case class WaitState(
                        Seconds: Option[Long] = None,
                        SecondsPath: Option[String] = None,
                        Timestamp: Option[ZonedDateTime] = None,
                        TimestampPath: Option[String] = None,
                        follow: Follow,
                        InputPath: Option[String] = None,
                        OutputPath: Option[String] = None,
                        Comment: Option[String] = None
                      ) extends State {
    override def accept[T](visitor: StateVisitor[T]): T =
      visitor.visit(this)
  }

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

  object FailState {
    val format: Format[FailState] = Json.format[FailState]
  }

  case class FailState(
                        Comment: Option[String] = None
                      ) extends State {
    override def accept[T](visitor: StateVisitor[T]): T =
      visitor.visit(this)
  }

  object ParallelState {
    val format: Format[ParallelState] = Json.format[ParallelState]
  }

  case class ParallelState(
                            follow: Follow,
                            InputPath: Option[String] = None,
                            OutputPath: Option[String] = None,
                            ResultPath: Option[String] = None,
                            Retry: Option[List[Retrier]],
                            Catch: Option[List[Catcher]],
                            Comment: Option[String] = None
                          ) extends State {
    override def accept[T](visitor: StateVisitor[T]): T =
      visitor.visit(this)
  }

}