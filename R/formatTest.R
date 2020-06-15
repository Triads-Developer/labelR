#' Format a MTurk qualification test
#'
#' \code{formatTest} formats the XML code required to make a MTurk qualification
#' test.
#'
#' @param title A string containing the title of the qualification test, as it
#' appears within the test.
#' @param instruction_overview A string providing general instructions for the HITs
#' @param instruction_list A nested list of strings containing bulleted instructions
#' and examples. Each element of the list object corresponds to a separate bullet
#' in the list of instructions. Nesting list objects within one another will allow
#' for nested bullets.
#' @param question_prompt A string containing the common question prompt used
#' across all pairwise comparison questions.
#' @param practice_overview A string providing general instructions for the
#' practice questions within the qualification test. This should inform workers
#' how practice questions differ from other questions in the test and/or HITs
#' on MTurk.
#' @param practice_questions A list of lists where each element of the main list
#' contains a single list for each unique practice question. Each question-specific
#' list contains three elements, where the first and second element
#' contain the left and right comparison text, respectively, and the third
#' element contains text explaining the correct answer.
#' @param test_overview A string providing general instructions for the
#' test questions within the qualification test. This should contain information
#' about scoring procedures.
#' @param test_questions A list of lists where each element of the main list
#' contains a single list for each unique practice question. Each question-specific
#' list contains three elements, where the first and second element
#' contain the left and right comparison text, respectively, and the third
#' element contains an integer corresponding to the index of the correct choice.
#' @param closing_message A string containing the final text presented in the
#' training module. This should contain warnings about worker monitoring and/or
#' rate limiting procedures.
#' @param save_files A logical indicating whether the .xml files for the 
#' qualification test and answer key should be saved to the working directory.
#'
#' @details
#' The function writes generic .xml files required to make an MTurk qualification
#' test. Though the function is designed for users to pass strings directly into
#' the arguments, the arguments also accept HTML-formatted text.
#'
#' @return A list of XML formatted strings that specify the qualification test
#' and answer key. By default, each is written as a .xml file in the working directory for
#' users to make manual revisions.
#'
#' @author Ryden Butler
#'
#' @rdname formatTest
#' @export

formatTest <- function(title,
                       instruction_overview,
                       instruction_list,
                       question_prompt,
                       practice_overview,
                       practice_questions,
                       test_overview,
                       test_questions,
                       closing_message,
                       save_files = T){

  n_practice <- length(practice_questions)
  n_test <- length(test_questions)

  formatted_instruction_list <- paste0('<ul>', listify(instruction_list), '</ul>')

  formatted_practice <- paste0(sapply(1:length(practice_questions), function(x){
    formatQuestion(practice_questions[[x]], x, question_prompt, test_question = F)
  }), collapse = '')

  formatted_test <- paste0(sapply(1:length(test_questions), function(x){
    formatQuestion(test_questions[[x]], x, question_prompt, test_question = T)
  }), collapse = '')

  instructions <- paste0("<h2>Instructions</h2><p>",
                         instruction_overview,
                         "</p> <h2>Here are a few rules of thumb to guide you</h2>",
                         formatted_instruction_list)

  question_form <- paste0(
  "<QuestionForm xmlns=\"http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/QuestionForm.xsd\">
     <Overview>
       <Title>", title, "</Title>
         <FormattedContent><![CDATA[", instructions, "]]></FormattedContent>
      </Overview>
      <Overview>
        <FormattedContent><![CDATA[
          <br></br>
          <h2>The next ", n_practice, " questions are example HITs</h2><p>",
          practice_overview,
          "</p><br></br>]]></FormattedContent>
      </Overview>",
      formatted_practice,
      "<Overview>
        <FormattedContent><![CDATA[
          <br></br>
          <h2>The next ", n_test, " questions are your test HITs.</h2><p>",
          test_overview,
          "</p><br></br>]]></FormattedContent>
       </Overview>",
       formatted_test,
       "<Overview>
        <FormattedContent><![CDATA[
          <h2>Before you submit your answers...</h2>
          <br></br>
          <p><b>You will only have 1 chance to take this test.</b> Make sure that you are satisfied with all of your answers above before submitting.
          <br></br>", closing_message, "</p>]]></FormattedContent>
      </Overview>
  </QuestionForm>")

  writeLines(question_form, "QualTest.xml")

  formatted_answers <- paste0(sapply(1:length(test_questions), function(x){
    formatAnswers(test_questions[[x]], x)
  }), collapse = '')

  answer_form <- paste0("<AnswerKey xmlns=\"http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/AnswerKey.xsd\">",
                       formatted_answers,
                       "</AnswerKey>")

  writeLines(answer_form, "AnswerKey.xml")
  return(list(Questions = question_form,
              Answers = answer_form))
}
