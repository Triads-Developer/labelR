#' Send comparisons to Mechanical Turk
#'
#' \code{sendComparisons} uploads comparisons onto MTurk for labeling.
#'
#' @param hit_type The HITType ID from the MTurk requester dashboard.
#' @param hit_layout The Layout ID from the MTurk requester dashboard.
#' @param comparisons_to_send A data.frame of two columns, where each row
#' corresponds with a single comparison and each entry is the text of the
#' documents to be compared.
#' @param expire_in_seconds A string indicating the number of seconds until the
#' batch of HITs expires. The default is 8 hours (28,800 seconds). The value must
#' be between 30 and 31,536,000.
#' @param n_assignments A string indicating the number of times each unique
#' comparison is to be uploaded. The default is '1'.
#' @param hit_param_names A vector of strings with parameter names as they
#' appear in the HITformat.html file. These should not be modified unless
#' using a custom version of HITformat.html where alternate parameter names have
#' been specified.
#'
#' @details
#' The function sends comparisons to MTurk for labeling.
#'
#' @return A vector of HIT ids corresponding to the most recent batch sent to MTurk.
#'
#' @author Ryden Butler
#'
#' @rdname sendComparisons
#' @import 'pyMTurkR'
#' @export

sendComparisons <- function(hit_type = NULL,
                            hit_layout = NULL,
                            comparisons_to_send = NULL,
                            expire_in_seconds = as.character(60 * 60 * 8),
                            n_assignments = "3",
                            hit_param_names = c("doc1", "doc2")) {
  current_HIT_ids <- rep(NA, nrow(comparisons_to_send))
  message("Sending task to MTurk")
  for (i in 1:nrow(comparisons_to_send)) {
    hit_params <- list()
    for (j in 1:length(hit_param_names)) {
      hit_params[[j]] <- list(
        Name = hit_param_names[j],
        Value = comparisons_to_send[i, j]
      )
      print(hit_params[[j]])
    }
    current_HIT_ids[i] <- suppressMessages(CreateHIT(
      hit.type = hit_type,
      hitlayoutid = hit_layout,
      hitlayoutparameters = hit_params,
      assignments = n_assignments,
      expiration = expire_in_seconds,
      # annotation = batch_annotation,
      verbose = FALSE
    ))$HITId
     print(current_HIT_ids)
  }
  return(current_HIT_ids)
}
