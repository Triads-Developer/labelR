#' Format a MTurk qualification test question
#'
#' \code{formatQuestion} formats the XML code required to make a pairwise comparison
#' question for a MTurk qualification test.
#'
#' @param question_list A list object where the first and second element
#' contain the left and right comparison text, respectively, and the third
#' element contains an integer corresponding to the index of the correct choice.
#' @param question_number An integer referencing the question number as it
#' appears in the MTurk qualification test.
#' @param question_prompt A string containing the common question prompt used
#' across all pairwise comparison questions.
#' @param test_question A logical where FALSE indicates that the question is
#' a practice question and TRUE indicates that the question is a test question.
#'
#'
#' @details
#' The function automatically formats the XML code required to make a generic
#' pairwise comparison question for an MTurk qualification test.
#'
#' @return A string of XML code that constructs a question for an
#' MTurk qualification test.
#'
#' @author Ryden Butler
#'
#' @rdname formatQuestion
#' @export

formatQuestion <- function(question_list,
                           question_number,
                           question_prompt,
                           test_question = F){

  is_required <- ifelse(test_question, 'true', 'false')
  question_type <- ifelse(test_question, 'Test', 'Example')
  question_abbrev <- ifelse(test_question, 'T', 'E')

  question <- paste0(
  "<Question>
     <QuestionIdentifier>", question_type, question_number, "</QuestionIdentifier>
     <DisplayName>", question_abbrev, question_number, "</DisplayName>
     <IsRequired>", is_required, "</IsRequired>
     <QuestionContent>
       <Text> ",  question_type, ' ', question_number, ": ", question_prompt, "</Text>
     </QuestionContent>
     <AnswerSpecification>
       <SelectionAnswer>
       <StyleSuggestion>radiobutton</StyleSuggestion>
         <Selections>
           <Selection>
           <SelectionIdentifier>left</SelectionIdentifier>
           <FormattedContent><![CDATA[
             <p>", question_list[[1]], "</p>]]></FormattedContent>
           </Selection>
           <Selection>
           <SelectionIdentifier>right</SelectionIdentifier>
           <FormattedContent><![CDATA[
             <p>", question_list[[2]], "</p>]]></FormattedContent>
           </Selection>
         </Selections>
       </SelectionAnswer>
     </AnswerSpecification>
   </Question>",
   ifelse(test_question,
          "",
          paste0("<Overview>
            <FormattedContent><![CDATA[
            <b>Answer: ", question_list[[3]], "</b>]]></FormattedContent>
          </Overview>"))
   )
  return(question)
}
