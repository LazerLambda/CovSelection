source("MB_DGF.R")


test_equalize_edges <- function(n = 100, upper = 4) {
  m <- matrix(sample(c(TRUE,FALSE), n^2, TRUE), nrow = n, ncol = n)
  m[upper.tri(m, diag = TRUE)] <- NA
  m <- equalize_edges(m, upper = upper)
  checkmate::assert(!(TRUE %in% (apply(m, 1, sum, na.rm = T) > upper)))
  checkmate::assert(!(TRUE %in% (apply(m, 2, sum, na.rm = T) > upper)))
}


run_test <- function(){
  for(i in seq_along(5)) mb_gen(100, 500, T)
}