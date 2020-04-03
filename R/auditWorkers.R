#' Audit Mechanical Turk workers
#'
#' \code{auditWorkers} estimates worker quality scores from collected MTurk data.
#'
#' @param current_experiment_results An object returned from \code{getResults}
#' containing HIT results from Mechanical Turk.
#' @param reference_results An optional object of HIT results from previous
#' experiments. When available, this improves the accuracy of worker quality scores.
#'
#' @details
#' The function audits worker quality using a Bayesian hierarchical model. Worker
#' means less than one are generally considered poor quality, though means can be
#' unreliable for workers with a low number of total HITs.
#'
#' @return A dataframe containing workers' posterior means and HIT frequencies.
#'
#' @author Ryden Butler
#'
#' @rdname auditWorkers
#' @import 'sentimentIt'
#' @export
#'
auditWorkers <- function(current_experiment_results,
                         reference_results = NULL){
  if(is.null(reference_results)){
    CombinedResults <- current_experiment_results
  } else {
    CombinedResults <- rbind(reference_results,
                             current_experiment_results)
  }

  STAN <- fitStan(data = CombinedResults)
  Workers <- checkWorkers(stan_fit = STAN$fit, data = CombinedResults)
  Counts <- table(as.character(CombinedResults$worker_id[(CombinedResults$worker_id %in% current_experiment_results$worker_id)]))/2
  Means <- data.frame(Workers$worker_posteriors[(Workers$worker_posteriors$workers %in% current_experiment_results$worker_id), ])

  out <- data.frame(cbind(Means[, 1:2], Counts[match(Means$workers, names(Counts))])[sort.list(Means$mean) , c(1:2, 4)])
  print(out)
  return(out)
}
