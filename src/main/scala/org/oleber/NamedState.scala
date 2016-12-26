package org.oleber

import org.oleber.NamedState.StateName
import org.oleber.State.State
import play.api.libs.json._

import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

object NamedState {
  case class StateName(name: String)
  object StateName {
    implicit def fromString(name: String): StateName =
      StateName(name)

    implicit val format = new Format[StateName] {
      override def reads(json: JsValue): JsResult[StateName] =
        json.validate[String].map(StateName.apply)

      override def writes(o: StateName): JsValue =
        JsString(o.name)
    }
  }

  case class BaseNamedState[T <: State](name: StateName, state: T)
  object BaseNamedState {
    implicit def format[T <: State](implicit format: Format[T]): Format[BaseNamedState[T]] = Json.format[BaseNamedState[T]]
  }

  implicit def format[T <: State](implicit format: Format[T]) = new Format[NamedState[T]] {
    override def reads(json: JsValue): JsResult[NamedState[T]] =
      json.validate[BaseNamedState[T]].map(x => NamedState(x.name, Promise.successful(x.state)))

    override def writes(o: NamedState[T]): JsValue = {
      val state = o.promise.future.value match {
        case Some(Success(value)) => value
        case Some(Failure(exception)) => throw NamedStateException("Failed to evaluate State", exception)
        case None => throw NamedStateException(s"State not evaluated for ${o.name.name}")
      }
      Json.toJson(BaseNamedState(o.name, state))
    }
  }

  def apply[T <: State](name: StateName, state: T): NamedState[T] = {
    NamedState(name, promise = Promise.successful(state))
  }

  def apply[T <: State](name: StateName): NamedState[T] = {
    NamedState(name, promise = Promise[T]())
  }

  case class NamedStateException(msg: String, exception: Throwable = null) extends Exception(msg, exception)

}

case class NamedState[T <: State](name: StateName, promise: Promise[T])
