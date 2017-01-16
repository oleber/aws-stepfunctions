package org.oleber

import org.oleber.State.Follow.{End, Next}
import org.oleber.state.ChoiceState.{Choice, NumericGreaterThan, TopChoice}
import org.oleber.state.{ChoiceState, PassState, TaskState}
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

      val stateMachine = StateMachine(
        Comment = "A simple minimal example of the States language",
        StartAt = "Hello World",
        States = Map(
          "Hello World" -> TaskState(
            Resource = "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld",
            follow = End
          )
        )
      )

      Json.parse(jsonString) must_== Json.toJson(stateMachine)
    }
  }

  "A litle more complex" in {
    val jsonString =
      """
        |{
        |  "Comment": "An example of the Amazon States Language using a choice state.",
        |  "StartAt": "DispatcherState",
        |  "States": {
        |    "DispatcherState": {
        |      "Type": "Task",
        |      "Resource": "arn:aws:lambda:::function:stepFunctionDispatcher",
        |      "Next": "ChoiceState"
        |    },
        |    "ChoiceState": {
        |      "Type": "Choice",
        |      "Choices": [
        |        {
        |          "Variable": "$.size",
        |          "NumericGreaterThan": 1,
        |          "Next": "DownloaderState"
        |        }
        |      ],
        |      "Default": "Final State"
        |    },
        |    "DownloaderState": {
        |      "Type": "Task",
        |      "Resource": "arn:aws:lambda:::function:stepFunctionDownloader",
        |      "Next": "ChoiceState"
        |    },
        |    "Final State": {
        |      "Type": "Pass",
        |      "End": true
        |    }
        |  }
        |}
      """.stripMargin

    val stateMachine = StateMachine(
      Comment = "An example of the Amazon States Language using a choice state.",
      StartAt = "DispatcherState",
      States = Map(
        "DispatcherState" -> TaskState(
          Resource = "arn:aws:lambda:::function:stepFunctionDispatcher",
          follow = Next("ChoiceState")
        ),
        "ChoiceState" -> ChoiceState(
          Choices = List(
            TopChoice(
              NumericGreaterThan("$.size", 1),
              Next("DownloaderState")
            )
          ),
          Default = "Final State"
        ),
        "DownloaderState" -> TaskState(
          Resource = "arn:aws:lambda:::function:stepFunctionDownloader",
          follow = Next("ChoiceState")
        ),
        "Final State" ->PassState(End)
      )
    )

    Json.parse(jsonString) must_== Json.toJson(stateMachine)

  }
}