#' Get results from Mechanical Turk
#'
#' \code{getResults} downloads completed work from MTurk for the specified HITs
#' and can offer live progress updates on the amount of completed work.
#'
#' @param current_hit_ids A vector of HIT ids to be retieved.
#' @param current_document_ids A 2-column matrix of document ids corresponding
#' to the documents included in each HIT.
#' @param current_batch_id An integer used to associate the current results with
#' a desired batch
#' @param retry A logical where TRUE causes the function to repeat until all
#' results are downloaded, providing live progress updates throughout. When FALSE,
#' the function will only download all currently available results for the specified
#' HITs.
#' @param retry_in_seconds An integer specifying how long the function should pause
#' before retrying to collect results. Only applicable when \code{retry = T}.
#' @param hit_categories A vector of strings with crowd classifier category names
#' as they appear in the HITformat.html file. These should not be modified unless
#' using a custom version of HITformat.html where alternate category names have
#' been specified.
#'
#' @details
#' The function retrieves and formats labeled data from MTurk.
#'
#' @return A data.frame of results from MTurk.
#'
#' @author Ryden Butler
#'
#' @rdname getResults
#' @import 'pyMTurkR'
#' @export

getResults <- function(current_hit_ids,
                       current_document_ids,
                       current_batch_id = 0,
                       retry = T,
                       retry_in_seconds = 60,
                       hit_categories = c('Document 1', 'Document 2')){

  # check that amazon credentials & sandbox settings apply
  # if(nchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == 0){
  #   Sys.setenv(AWS_ACCESS_KEY_ID = AWS_id)
  #   Sys.setenv(AWS_SECRET_ACCESS_KEY = AWS_secret)
  # }
  # options(pyMTurkR.sandbox = sandbox)

  # convert all hit ids to character
  current_hit_ids <- as.character(current_hit_ids)

  # pre-format batch data for output
  batch_info <- data.frame(batch_id = rep(current_batch_id, ncol(current_document_ids) * nrow(current_document_ids)),
                           hit_id = as.vector(t(sapply(1:ncol(current_document_ids), function(x) current_hit_ids))),
                           document_id = as.vector(t(current_document_ids)),
                           stringsAsFactors = F)

  # extract unique hit ids to collect
  hit_ids <- batch_info$hit_id[seq(1, nrow(batch_info), ncol(current_document_ids))]

  # retrieve results from mturk
  turk_assignments <- NULL
  turk_answers <- NULL
  message('Getting HITs...')
  for(i in hit_ids){
    turk_data <- suppressMessages(GetAssignment(hit = i,
                                                get.answers = T))
    turk_assignments <- rbind(turk_assignments, turk_data$Assignments[ , c(1:3, 7)])
    turk_answers <- rbind(turk_answers, turk_data$Answers)
  }

  n_results <- nrow(turk_assignments)
  if(n_results == length(hit_ids)){
    message(paste0('All ', n_results, ' HITs retrieved'))
  } else {
    message(paste0(n_results, ' / ', length(hit_ids), ' results retrieved'))
    if(retry == T){
      Sys.sleep(retry_in_seconds)
      return(GetResults(current_batch_id,
                        current_hit_ids,
                        current_document_ids,
                        hit_categories,
                        retry,
                        retry_in_seconds,
                        AWS_id,
                        AWS_secret,
                        sandbox))
    }
  }
  results <- cbind(batch_info,
                   result = NA,
                   assignment_id = NA,
                   worker_id = NA,
                   completed_at = NA,
                   stringsAsFactors = F)[1:(n_results*ncol(current_document_ids)), ]

  answer_index <- sapply(turk_answers$FreeText, function(x) which(hit_categories == x))
  answer_index <- answer_index + cumsum(c(0, rep(ncol(current_document_ids), length(answer_index) - 1)))
  results$result <- 0
  results$result[answer_index] <- 1
  results$assignment_id <- as.vector(sapply(turk_assignments$AssignmentId, function(x) rep(x, ncol(current_document_ids))))
  results$worker_id <- as.vector(sapply(turk_answers$WorkerId, function(x) rep(x, ncol(current_document_ids))))
  results$completed_at <- as.vector(sapply(turk_assignments$SubmitTime, function(x) rep(x, ncol(current_document_ids))))
  results <- results[ , c(1, 5, 3, 4, 2, 6, 7)]
  return(results)
}
