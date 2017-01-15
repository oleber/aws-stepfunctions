package org.oleber

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}

import org.oleber.State.ChoiceState.{NumericGreaterThanEquals, _}
import org.oleber.State._
import org.oleber.State.Implicits._
import org.oleber.State.ParallelState.Branche
import org.specs2.mutable.Specification
import play.api.libs.json.Json
import play.api.libs.json.Json._

class StateSpec extends Specification {
  "State" should {
    "Pass State" in {
      val jsonString =
        """
          |{
          |  "Type": "Pass",
          |  "Result": {
          |    "x-datum": 0.381018,
          |    "y-datum": 622.2269926397355
          |  },
          |  "ResultPath": "$.coords",
          |  "Next": "End"
          |}
        """.stripMargin

      val state = PassState(
        Result = Some(obj(
          "x-datum" -> 0.381018,
          "y-datum" -> 622.2269926397355
        )),
        ResultPath = Some("$.coords"),
        follow = Follow.Next("End")
      )

      Json.parse(jsonString) must_== Json.toJson(state)
    }

    "Task State" in {
      val jsonString =
        """
          |{
          |  "Comment": "Task State example",
          |  "Type": "Task",
          |  "Resource": "arn:aws:swf:us-east-1:123456789012:task:HelloWorld",
          |  "Next": "NextState",
          |  "TimeoutSeconds": 300,
          |  "HeartbeatSeconds": 60
          |}
        """.stripMargin

      val state = TaskState(
        Comment = "Task State example",
        Resource = "arn:aws:swf:us-east-1:123456789012:task:HelloWorld",
        follow = Follow.Next("NextState"),
        TimeoutSeconds = 300,
        HeartbeatSeconds = 60
      )

      Json.parse(jsonString) must_== Json.toJson(state)
    }

    "Choice State" in {
      val jsonString =
        """
          |{
          |  "Type" : "Choice",
          |  "Choices": [
          |    {
          |        "Not": {
          |          "Variable": "$.type",
          |          "StringEquals": "Private"
          |        },
          |        "Next": "Public"
          |    },
          |    {
          |      "And": [
          |        {
          |          "Variable": "$.value",
          |          "NumericGreaterThanEquals": 20
          |        },
          |        {
          |          "Variable": "$.value",
          |          "NumericLessThan": 30
          |        }
          |      ],
          |      "Next": "ValueInTwenties"
          |    }
          |  ],
          |  "Default": "DefaultState"
          |}
        """.stripMargin

      val state = ChoiceState(
        Choices = List(
          TopChoice(
            choice = Not(StringEquals("$.type", "Private")),
            Next = "Public"
          ),
          TopChoice(
            choice = And(List(
              NumericGreaterThanEquals("$.value", 20l),
              NumericLessThan("$.value", 30l)
            )),
            Next = "ValueInTwenties"
          )
        ),
        Default = Some("DefaultState")
      )

      Json.parse(jsonString) must_== Json.toJson(state)
    }

    "Wait State" in {
      val jsonString =
        """
          |{
          |    "wait_ten_seconds" : {
          |        "Type" : "Wait",
          |        "Seconds" : 10,
          |        "Next": "NextState"
          |    },
          |    "wait_ten_seconds_path" : {
          |        "Type" : "Wait",
          |        "SecondsPath" : "$.expirydate",
          |        "Next": "NextState"
          |    },
          |    "wait_until" : {
          |        "Type": "Wait",
          |        "Timestamp": "2016-03-14T01:59:00Z[GMT]",
          |        "Next": "NextState"
          |    },
          |    "wait_until_path" : {
          |        "Type": "Wait",
          |        "TimestampPath": "$.expirydate",
          |        "Next": "NextState"
          |    }
          |}
        """.stripMargin

      val state = Map(
        "wait_ten_seconds" -> WaitState.Seconds(10, Follow.Next("NextState")),
        "wait_ten_seconds_path" -> WaitState.SecondsPath("$.expirydate", Follow.Next("NextState")),
        "wait_until" -> WaitState.Timestamp(ZonedDateTime.of(2016, 3, 14, 1, 59, 0, 0, ZoneId.of("GMT")), Follow.Next("NextState")),
        "wait_until_path" -> WaitState.TimestampPath("$.expirydate", Follow.Next("NextState"))
      )

      Json.parse(jsonString) must_== Json.toJson(state)
    }

    "Succeed State" in {
      val jsonString =
        """
          |{
          |  "Type": "Succeed"
          |}
        """.stripMargin

      val state = SucceedState()

      Json.parse(jsonString) must_== Json.toJson(state)
    }

    "Fail State" in {
      val jsonString =
        """
          |{
          |  "Type": "Fail",
          |  "Error": "ErrorA",
          |  "Cause": "Kaiju attack"
          |}
        """.stripMargin

      val state = FailState(
        Error = "ErrorA",
        Cause = "Kaiju attack"
      )

      Json.parse(jsonString) must_== Json.toJson(state)
    }

    "Parallel State" in {
      val jsonString =
        """
          |{
          |  "Type": "Parallel",
          |  "Branches": [
          |    {
          |      "StartAt": "LookupAddress",
          |      "States": {
          |        "LookupAddress": {
          |          "Type": "Task",
          |          "Resource":
          |            "arn:aws:lambda:us-east-1:123456789012:function:AddressFinder",
          |          "End": true
          |        }
          |      }
          |    },
          |    {
          |      "StartAt": "LookupPhone",
          |      "States": {
          |        "LookupPhone": {
          |          "Type": "Task",
          |          "Resource":
          |            "arn:aws:lambda:us-east-1:123456789012:function:PhoneFinder",
          |          "End": true
          |        }
          |      }
          |    }
          |  ],
          |  "Next": "NextState"
          |}
        """.stripMargin

      val state = ParallelState(
        Branches = List(
          Branche(
            StartAt = "LookupAddress",
            States = Map(
              "LookupAddress" -> TaskState(
                Resource = "arn:aws:lambda:us-east-1:123456789012:function:AddressFinder",
                follow = Follow.End
              )
            )
          ),
          Branche(
            StartAt = "LookupPhone",
            States = Map(
              "LookupPhone" -> TaskState(
                Resource = "arn:aws:lambda:us-east-1:123456789012:function:PhoneFinder",
                follow = Follow.End
              )
            )
          )
        ),
        follow = Follow.Next("NextState")
      )

      Json.parse(jsonString) must_== Json.toJson(state)
    }
  }
}
