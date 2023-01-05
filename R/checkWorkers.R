#' Detect outlying workers
#'
#' @details
#'
#' 
#' Once you have estimated a Stan model, you can use this to identify workers who are unreliable. You can use the
#' output from the Stan model, as well as the output from the completed comparisons, to identify unreliable workers.
#' Use the `checkWorkers()` function to detect these outlying workers. This function returns a vector of IDs
#' identifying workers who are unreliable.
#'
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#'
#'
#' @param stan_fit A stan fit.
#' @param data The data used to fit stan.
#' @param cut_point A cutoff point to classify posterior coefficients. The proportion of posterior coefficients below \code{cut_point} is used to determine outliers. (Default is 1)
#' @param cut_proportion A cutoff proportion of posterior coefficients below \code{cut_point}. If the proportion of posterior coefficients below \code{cut_points} is higher than \code{cut_proportion}, a worker will be considered as an outlier provided that she answers more than the number of questions in \code{n.questions}. (Default is 0.9)
#' @param n.questions The number of questions to consider in order to determine suggested banned workers. (Default is 50)
#' @param plot_hist If TRUE, plot the histogram of workers with a rug plot. Default is FALSE
#' @param hist_path Save the histogram to path and file name specified. Default is NULL and no plot is saved.
#'
#' @return ban_workers A vector of outlying workers' IDs whose proportion of posterior coefficients below \code{cut_point} is greater than \code{cut_proportion} and who answered more than the number of questions in \code{n.questions}
#'
#' @examples
#' \dontrun{
#' ban_workers <- checkWorkers(stan_fit = fit, data = output)
#' }
#'
#' @author David Carlson
#'
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname checkWorkers
#'
#' @export
checkWorkers <- function(stan_fit, data, cut_point = 1, cut_proportion = 0.9,
                         n.questions = 50, plot_hist = FALSE, hist_path = NULL) {
  requireNamespace("rstan") # bug in rstan - needs explicit call
  if (class(stan_fit) != "stanfit") {
    stop("fit should be class stanfit")
  }

  if (dim(data)[2] != 7) {
    stop("data dimension mismatches")
  }

  if (!("worker_id" %in% colnames(data))) {
    stop("worker_id is not in data")
  }

  if (!is.numeric(cut_point)) {
    stop("cut_point should be numeric")
  }

  if (cut_proportion < 0 | cut_proportion > 1) {
    stop("cut_proportion should be in the range between 0 to 1")
  }

  bs <- rstan::extract(stan_fit)[["b"]]
  bs_proportion <- vapply(
    1:dim(bs)[2],
    function(p) {
      length(which(bs[, p] < cut_point))
    }, 1
  ) / dim(bs)[1]

  workers <- levels(as.factor(data$worker_id[seq(1, dim(data)[1], by = 2)]))
  j <- as.numeric(as.factor(data$worker_id[seq(1, dim(data)[1], by = 2)]))
  ban_workers <- workers[which(bs_proportion > cut_proportion & table(j) > n.questions)]

  worker_df <- rstan::summary(stan_fit)$summary[grep("b", rownames(rstan::summary(stan_fit)$summary)), ]

  if (!is.null(hist_path)) {
    pdf(hist_path)
    hist(worker_df[, "mean"], main = "Histogram of Worker Estimates", xlab = "Binned Estimates")
    rug(worker_df[, "mean"])
    dev.off()
  }

  if (plot_hist) {
    plot(hist(worker_df[, "mean"]), main = "Histogram of Worker Estimates", xlab = "Binned Estimates")
    rug(worker_df[, "mean"])
  }
  worker_df <- cbind(workers, worker_df)

  return(list("ban_workers" = ban_workers, "worker_posteriors" = worker_df))
}
