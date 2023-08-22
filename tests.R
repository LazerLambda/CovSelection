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
  testthat::expect(
    !(TRUE %in% (apply(m, 1, sum, na.rm = T) > upper)),
    "Axis 1 does not fulfill criteria.")
  testthat::expect(
    !(TRUE %in% (apply(m, 2, sum, na.rm = T) > upper)),
    "Axis 2 does not fulfill criteria.")
}
run_test_equalize_edges()



run_test_mb_gen <- function(){
  for(i in seq_len(5)) mb_gen(100, 500)
}
run_test_mb_gen()




run_test_metrics <- function() {
  m <- as(matrix(c(1,0,0,0), nrow = 2, ncol = 2), "dsCMatrix")
  m_hat <- as(matrix(c(1,1,1,0), nrow = 2, ncol = 2), "dgCMatrix")
  res <- get_metrics(predicted = m_hat, actual = m)
  testthat::expect(round(res$precision, 2) == 0.33, "Precision not correct.")
  testthat::expect(res$recall == 1, "Recall not correct.")
  testthat::expect(res$f1 == 0.5, "F1 not correct.")
  
  m_hat <- as(matrix(c(1,0,0,0), nrow = 2, ncol = 2), "dgCMatrix")
  m <- as(matrix(c(1,1,1,0), nrow = 2, ncol = 2), "dsCMatrix")
  res <- get_metrics(predicted = m_hat, actual = m)
  testthat::expect(res$precision == 1, "Precision not correct.")
  testthat::expect(round(res$recall, 2) == 0.33, "Recall not correct.")
  testthat::expect(res$f1 == 0.5, "F1 not correct.")
  
  m_hat <- as(matrix(c(1,0,1,0), nrow = 2, ncol = 2), "dgCMatrix")
  m <- as(matrix(c(1,0,0,1), nrow = 2, ncol = 2), "dsCMatrix")
  res <- get_metrics(predicted = m_hat, actual = m)
  testthat::expect(res$precision == 0.5, "Precision not correct.")
  testthat::expect(res$recall == 0.5, "Recall not correct.")
  testthat::expect(res$f1 == 0.5, "F1 not correct.")
}
run_test_metrics()

run_test_metrics_2 <- function(){
  for (i in seq_len(10)){
    m <- matrix(sample(c(TRUE,FALSE), n^2, TRUE), nrow = n, ncol = n)
    m[upper.tri(m)] <- t(m)[upper.tri(m)]
    m <- as(m, "dsCMatrix")
    
    m_hat <- matrix(sample(c(TRUE,FALSE), n^2, TRUE), nrow = n, ncol = n)
    m_hat <- as(m_hat, "dgCMatrix")
    prec_pack <- Metrics::precision(as.vector(m), as.vector(m_hat))
    rec_pack <- Metrics::recall(as.vector(m), as.vector(m_hat))
    res <- get_metrics(predicted = m_hat, actual = m)
    testthat::expect(res$precision == prec_pack, "Precision not correct.")
    testthat::expect(res$recall == rec_pack, "Recall not correct.")
  }
}
run_test_metrics_2()