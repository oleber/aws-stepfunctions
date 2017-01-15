package org.oleber

import org.oleber.State.Follow
import org.oleber.state.TaskState
import org.specs2.mutable.Specification
import play.api.libs.json.Json

class StateMachineSpec extends Specification {

  import org.oleber.State.Implicits._

  "StateMachine" should {
    "Hello World" in {
      val jsonString =
        """
          |{
          |    "Comment": "A simple minimal example of the States language",
          |    "StartAt": "Hello World",
          |    "States": {
          |    "Hello World": {
          |      "Type": "Task",
          |      "Resource": "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld",
          |      "End": true
          |    }
          |  }
          |}
        """.
          stripMargin

      val rawState = StateMachine(
        Comment = "A simple minimal example of the States language",
        StartAt = "Hello World",
        States = Map(
          "Hello World" -> TaskState(
            Resource = "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld",
            follow = Follow.End
          )
        )
      )

      Json.parse(jsonString) must_== Json.toJson(rawState)
    }
  }
}