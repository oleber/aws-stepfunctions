package org.oleber.state

import java.time.ZonedDateTime

import org.oleber.State.{Follow, State, StateVisitor}
import play.api.libs.json.{Format, Json}

object WaitState {
  import org.oleber.State.Implicits._

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