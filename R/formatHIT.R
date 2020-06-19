#' Format a MTurk HIT
#'
#' \code{formatHIT} formats the HTML code required to make a MTurk HIT.
#'
#' @param question_prompt A string containing the common question prompt used
#' across all pairwise comparison questions.
#' @param instruction_overview A string providing general instructions for the HITs
#' @param instruction_list A nested list of strings containing bulleted instructions
#' and examples. Each element of the list object corresponds to a separate bullet
#' in the list of instructions. Nesting list objects within one another will allow
#' for nested bullets.
#' @param short_instructions A nested list of lists containing bulleted instructions
#' for HITs. This should be an abridged version of \code{instruction_list} that is
#' displayed within the HIT's short instruction tab.
#' @param closing_message A string containing the final text presented in the
#' training module. This should contain warnings about worker monitoring and/or
#' rate limiting procedures.
#' @param doc_labels A vector of strings indicating the labels to be assigned to 
#' each document.
#' @param file_name A string containing the desired name of the output HTML file.
#' @details
#' The function writes a generic .html file required to make a MTurk HIT. Though
#' the function is designed for users to pass strings directly into
#' the arguments, the arguments also accept HTML-formatted text.
#'
#' @return A string of HTML code that formats a HIT for MTurk. This is also
#' written as a .html file in the working directory for users to make manual
#' revisions.
#'
#' @author Ryden Butler
#'
#' @rdname formatHIT
#' @export

formatHIT <- function(question_prompt,
                      instruction_overview,
                      instruction_list,
                      short_instruction_list,
                      closing_message,
                      doc_labels = c('Document 1', 'Document 2'),
                      file_name = 'HIT.html'){

  short_template <- paste0(sapply(short_instruction_list, function(x){
    paste0("<h3>", x[1], "</h3>",
           paste0("<p>",
                  paste0(sapply(x[-1],
                                function(i){
                                  paste0("- ", i , "<br><br>")
                                  }), collapse = ''),
                  "</p>"))
    }), collapse = '')

  formatted_instruction_list <- paste0('<ul>', listify(instruction_list), '</ul>')

  instructions <- paste0("<h2>Instructions</h2><p>",
                         instruction_overview,
                         "</p> <h2>Here are a few rules of thumb to guide you</h2>",
                         formatted_instruction_list)
  hit <- paste0(
  "<!-- You must include this JavaScript file -->
   <script src=\"https://assets.crowd.aws/crowd-html-elements.js\"></script>

   <!-- For the full list of available Crowd HTML Elements and their input/output documentation,
      please refer to https://docs.aws.amazon.com/sagemaker/latest/dg/sms-ui-template-reference.html -->

   <!-- You must include crowd-form so that your task submits answers to MTurk -->
   <crowd-form answer-format=\"flatten-objects\">

   <!-- The crowd-classifier element will create a tool for the Worker to select the
           correct answer to your question -->
    <crowd-classifier
      name=\"equal\"
      categories=\"[", doc_labels[1], ", ", doc_labels[2], "]\"
      header=", paste0("\"", question_prompt, "\""),
    ">
        <classification-target>
            <div>
                <h3>", doc_labels[1], "</h3>
	      	<p>
                   <!-- The first document text will be substituted for the doc1 variable below -->
                    ${doc1}
                </p>
	    </div>
	    <hr/>
            <div>
                <h3>", doc_labels[2], "</h3>
	      	<p>
                   <!-- The second document text will be substituted for the doc2 variable below -->
                    ${doc2}
                <br>
                </p>
	    </div>
      </classification-target>

       <!-- Use the short-instructions section for quick instructions that the Worker
              will see while working on the task. Including some basic examples of
              good and bad answers here can help get good results. You can include
              any HTML here. -->
        <short-instructions>
          <h2>Rules:</h2>", short_template,
        "<h4>See full instructions above for more detail</h4>
      </short-instructions>

      <!-- Use the full-instructions section for more detailed instructions that the
            Worker can open while working on the task. Including more detailed
            instructions and additional examples of good and bad answers here can
            help get good results. You can include any HTML here. -->
      <full-instructions>",
        instructions,
      "<p>", closing_message, "</p>
      </full-instructions>
    </crowd-classifier>
</crowd-form>")
  writeLines(hit, file_name)
  print(paste0("HIT format written to file: ", file_name))
  return(hit)
}
