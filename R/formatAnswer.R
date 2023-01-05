#' Format a MTurk qualification test answer key
#'
#' \code{formatAnswer} formats the XML code required to make an answer key for
#' an MTurk qualification test.
#'
#' @param question_list A list object where the first and second element
#' correspond to the left and right comparison text, respectively, and the third
#' element corresponds to the index of the correct choice.
#' @param question_number An integer referencing the question number as it
#' appears in the MTurk qualification test.
#'
#' @details
#' The function automatically formats the XML code required to make a generic
#' answer key for an MTurk qualification test.
#'
#' @return A string of XML code that constructs the answer key for an
#' MTurk qualification test.
#'
#' @author Ryden Butler
#'
#' @rdname formatAnswer
#' @export

formatAnswers <- function(question_list,
                          question_number) {
  answer <- paste0(
    "<Question>
     <QuestionIdentifier>Test", question_number, "</QuestionIdentifier>
    <AnswerOption>
      <SelectionIdentifier>left</SelectionIdentifier>
      <AnswerScore>", ifelse(question_list[[3]] == 1, 1, 0), "</AnswerScore>
    </AnswerOption>
    <AnswerOption>
      <SelectionIdentifier>right</SelectionIdentifier>
      <AnswerScore>", ifelse(question_list[[3]] == 1, 0, 1), "</AnswerScore>
    </AnswerOption>
  </Question>"
  )

  return(answer)
}
