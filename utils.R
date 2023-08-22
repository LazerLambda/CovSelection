# Script including helper functions for the main experiment.
# 
# Neighborhood Selection as Covariance Selection - Revisiting Meinshausen and
# Bühlmann's Approach
#
# For the seminar "Network Learning and Sparse Estimation"
#
# Philipp Koch, 2023

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
  
  precision <- ifelse(
    test = (tp + fp) == 0, 
    yes = 0,
    no = tp / (tp + fp))
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


