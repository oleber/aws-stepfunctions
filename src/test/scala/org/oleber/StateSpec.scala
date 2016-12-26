package org.oleber

import org.oleber.State.{Follow, PassState, TaskState}
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

//    "Task State" in {
//      val jsonString =
//        """
//          |{
//          |  "Comment": "Task State example",
//          |  "Type": "Task",
//          |  "Resource": "arn:aws:swf:us-east-1:123456789012:task:HelloWorld",
//          |  "Next": "NextState",
//          |  "TimeoutSeconds": 300,
//          |  "HeartbeatSeconds": 60
//          |}
//        """.stripMargin
//
//      val state = TaskState(
//        Comment = "Task State example",
//        Type = "Task",
//        Resource = "arn:aws:swf:us-east-1:123456789012:task:HelloWorld",
//        Next = "NextState",
//        TimeoutSeconds = 300,
//        HeartbeatSeconds = 60
//      )
//
//      Json.parse(jsonString) must_== Json.toJson(state)
//    }

  }

}
