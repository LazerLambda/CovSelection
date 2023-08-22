# Script including test functions for the other scripts.
# 
# Neighborhood Selection as Covariance Selection - Revisiting Meinshausen and
# Bühlmann's Approach
#
# For the seminar "Network Learning and Sparse Estimation"
#
# Philipp Koch, 2023


source("MB_DGF.R")
source("./utils.R")


run_test_equalize_edges <- function(n = 100, upper = 4) {
  m <- matrix(sample(c(TRUE,FALSE), n^2, TRUE), nrow = n, ncol = n)
  m[upper.tri(m, diag = TRUE)] <- NA
  m <- equalize_edges(m, upper = upper)
  checkmate::assert(!(TRUE %in% (apply(m, 1, sum, na.rm = T) > upper)))
  checkmate::assert(!(TRUE %in% (apply(m, 2, sum, na.rm = T) > upper)))
}
run_test_equalize_edges()



run_test_mb_gen <- function(){
  for(i in seq_along(5)) mb_gen(100, 500)
}
run_test_mb_gen()




run_test_metrics <- function() {
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
run_test_metrics()
