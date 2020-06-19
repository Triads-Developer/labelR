#' Audit Mechanical Turk workers
#'
#' \code{auditWorkers} estimates worker quality scores from collected MTurk data.
#'
#' @param current_experiment_results An object returned from \code{getResults}
#' containing HIT results from Mechanical Turk.
#' @param reference_results An optional object of HIT results from previous
#' experiments. When available, this improves the accuracy of worker quality scores.
#' @param exclude_workers An optional object of worker ids to be excluded from 
#' the printed output. Typically, a vector of ids for banned worker is passed here 
#' to prevent them from continually appearing in audits.
#' @param plot_audit When TRUE, the function will produce a plot of worker ability 
#' estimates and their corresponding 95% credible intervals.
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
                         reference_results = NULL,
                         exclude_workers = NULL,
                         plot_audit = F){
  if(is.null(reference_results)){
    CombinedResults <- current_experiment_results
  } else {
    CombinedResults <- rbind(reference_results,
                             current_experiment_results)
  }

  STAN <- fitStan(data = CombinedResults)
  Workers <- checkWorkers(stan_fit = STAN$fit, data = CombinedResults)
  Counts <- table(current_experiment_results$worker_id)/2
  Means <- Workers$worker_posteriors[(Workers$worker_posteriors[ , 1] %in% current_experiment_results$worker_id), ]

  result <- data.frame(cbind(Means[, c(1:2, 5, 9)],
                          Counts[match(Means[ , 1], names(Counts))]),
                    stringsAsFactors = F)[sort.list(Means[ , 2], decreasing = T), ]
  result[ , -1] <- apply(result[ , -1], 2, as.numeric)
  rownames(result) <- NULL
  colnames(result) <- c('WorkerID', 'CoderAbility', '2.5%CI', '97.5%CI', 'nHITs')
  out <- result[!(result[ , 1] %in% exclude_workers), ]
  print(out)
  if(plot_audit){
    plot(x = 1:nrow(out),
         y = out[ , 2],
         xlab = '',
         ylab = 'Ability',
         las = 1,
         ylim = c(0, max(out[ , 4])),
         axes = F, cex = 0.5)
    axis(1, at = 1:nrow(out), labels = out[ , 1], cex.axis = 0.5, 
         las = 2,  lwd = 0, line = -0.5)
    axis(2, at = 0:ceiling(max(out[ , 4])), las = 1)
    points(x = 1:nrow(out),
           y = out[ , 3],
           pch = '-')
    points(x = 1:nrow(out),
           y = out[ , 4],
           pch = '-')
    abline(h = 1, lty = 2, col = 'blue')
  }
  return(result)
}
