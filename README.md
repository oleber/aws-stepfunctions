# aws-stepfunctions
Template generator for Aws Step Functions. The intention is to have the scala compiler verifing the format. 

## Simple example
The Hello World example:

```JSON
{
  "Comment": "A simple minimal example of the States language",
  "StartAt": "Hello World",
  "States": {
  "Hello World": {
    "Type": "Task",
    "Resource": "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld",
    "End": true
  }
}
```
  
Can be writen has

```scala
StateMachine(
  Comment = "A simple minimal example of the States language",
  StartAt = "Hello World",
  States = Map(
    "Hello World" -> TaskState(
      Resource = "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld",
      follow = End
    )
  )
)
```

## Do you want to use it?

Just copy the code. I did it ;)
