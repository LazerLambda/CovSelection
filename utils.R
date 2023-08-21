library(checkmate)


lw <- function(arg) return(length(Matrix::which(arg)))

#' `get_metrics` Function
#
#' Compute precision, recall and f1 measure.
#' Based on Matrix-package.
#' @param predicted Predicted precision matrix.
#' @param actual Ground truth precision matrix.
#' @return list including values of *precision*, *recall*, *f1*, *hamming*.
get_metrics <- function(predicted, actual) {
  checkmate::assert_class(predicted, "dgCMatrix")
  checkmate::assert_class(actual, "dsCMatrix")

  predicted_mask <- as(predicted, "lgCMatrix")
  actual_mask <- as(actual, "lsCMatrix")
  
  tp <- lw((actual_mask & predicted_mask))
  fp <- lw((!actual_mask & predicted_mask))
  fn <- lw((actual_mask & !(actual_mask & predicted_mask)))
  
  log_info("tp: {tp}, fp: {fp}, fn: {fn}")
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * precision * recall / (precision + recall)
  hamming_dist <- sum(actual_mask != predicted_mask)
  return(
    list(
      precision = precision,
      recall = recall,
      f1 = f1,
      hamming_dist = hamming_dist
      )
    )
}


# TEST
run_test <- function() {
  m <- as(matrix(c(1,0,0,0), nrow = 2, ncol = 2), "dsCMatrix")
  m_hat <- as(matrix(c(1,1,1,0), nrow = 2, ncol = 2), "dgCMatrix")
  res <- get_metrics(predicted = m_hat, actual = m)
  checkmate::assert(round(res$precision, 2) == 0.33)
  checkmate::assert(res$recall == 1)
  checkmate::assert(res$f1 == 0.5)
  
  m_hat <- as(matrix(c(1,0,0,0), nrow = 2, ncol = 2), "dgCMatrix")
  m <- as(matrix(c(1,1,1,0), nrow = 2, ncol = 2), "dsCMatrix")
  res <- get_metrics(predicted = m_hat, actual = m)
  checkmate::assert(res$precision == 1)
  checkmate::assert(round(res$recall, 2) == 0.33)
  checkmate::assert(res$f1 == 0.5)
  
  m_hat <- as(matrix(c(1,0,1,0), nrow = 2, ncol = 2), "dgCMatrix")
  m <- as(matrix(c(1,0,0,1), nrow = 2, ncol = 2), "dsCMatrix")
  res <- get_metrics(predicted = m_hat, actual = m)
  checkmate::assert(res$precision == 0.5)
  checkmate::assert(res$recall == 0.5)
  checkmate::assert(res$f1 == 0.5)
}

