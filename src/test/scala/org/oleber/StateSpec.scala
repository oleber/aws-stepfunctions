package org.oleber

import org.oleber.State.ChoiceState.{NumericGreaterThanEquals, _}
import org.oleber.State._
import org.oleber.State.Implicits._
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
              NumericGreaterThanEquals("$.value", 20),
              NumericLessThan("$.value", 30)
            )),
            Next = "ValueInTwenties"
          )
        ),
        Default = Some("DefaultState")
      )

      Json.parse(jsonString) must_== Json.toJson(state)
    }
  }
}
