# Implementation of the Data-Generating-Process (DGP) of Meinshausen and
# Bühlmann (2006).
# 
# Neighborhood Selection as Covariance Selection - Revisiting Meinshausen and
# Bühlmann's Approach
#
# For the seminar "Network Learning and Sparse Estimation"
#
# Philipp Koch, 2023


library(MASS)



#' Helper Function to check Whether Nodes fulfill Conditions.
#' 
#' @param axis Axis for traversal.
#' @param m Matrix on which conditions must be fulfilled (lower triangular,
#' logical matrix).
#' @param upper Upper bound for check.
#' @returns Logical vector for rows/columns.
edge_cond_met <- function(axis, m, upper) {
  (apply(m, axis, sum, na.rm=TRUE) > upper)
}



#' Helper Function to Randomly Set an Entry to Zero.
#' 
#' @param e Element (vector, row/column of matrix).
#' @returns Updated vector where an element is set to FALSE. 
set_edge_false <- function(e) {
  i <- sample(which(e), 1)
  e[i] <- FALSE
  e
}



#' Function to reduce edges for each node to max four.
#' 
#' Iterative procedure in which edges are randomly removed from the triangular
#' entries of the matrix. Procedure is applied on axis 1 and 2 subsequently.
#' It suffices to set the entry of the corresponding edge to FALSE.
#' 
#' @param m matrix (logical, lower triangular matrix where other values
#' including diagonal is set to NA).
#' @param upper Upper bound for edges corresponding to each node.
#' @returns Equalized matrix.
equalize_edges <- function(m, upper = 4) {
  ind_axis_one <- edge_cond_met(1, m, upper)
  ind_axis_two <- edge_cond_met(2, m, upper)
  while(
    TRUE %in% ind_axis_one ||
    TRUE %in% ind_axis_two) {
      if (TRUE %in% ind_axis_one) {
        if(is.null(dim(m[ind_axis_one, ]))){
          i <- sample(which(m[ind_axis_one, ]), 1)
          m[ind_axis_one, ][i] <- FALSE
        } else {
          m[ind_axis_one, ] <- t(apply(m[ind_axis_one, ], 1, set_edge_false))
        }
        ind_axis_one <- edge_cond_met(1, m, upper)
      }

      if (TRUE %in% ind_axis_two) {
        if(is.null(dim(m[, ind_axis_two]))){
          i <- sample(which(m[, ind_axis_two]), 1)
          m[, ind_axis_two][i] <- FALSE
        } else {
          m[, ind_axis_two] <- apply(m[, ind_axis_two], 2, set_edge_false)
        }
        ind_axis_two <- edge_cond_met(2, m, upper)
      }
  }
  return(m)
}



#' `mb_gen` Function
#' 
#' Reproduce data-generating process from Meinshausen and Bühlmann (2006).
#' 
#' @param n number of observations.
#' @param d number of variables.
#' @param trnct flag to keep column of selection.
#' @return list object including the generated data.
mb_gen <- function(n, d, upper = 4) {
  # Sample from two-dim std. uniform distribution
  init_set <- data.frame(x = runif(d), y = runif(d))
  
  # Get distances
  m <- as.matrix(dist(init_set))
  
  # Get probabilities
  m <- dnorm(m / sqrt(d))
  
  # Choose with obtained probability
  m <- apply(m, c(1,2), function(e) {as.logical(rbinom(1,1, e))} )
  
  # Make matrix diagonal
  m[upper.tri(m, diag = TRUE)] <- NA
  
  # Set max edges for each node to `upper` (4).
  m <- equalize_edges(m, upper = upper)
  
  # Create symmetric matrix from lower triangle.
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  
  # Save adjacency matrix.
  theta <- m
  diag(theta) <- rep(0, d)
  theta <- as(theta, "dsCMatrix")
  
  # Create sigma.
  m <- 1 * m
  m <- 0.245 * m
  diag(m) <- rep(1, d)
  sigma <- solve(m) # TODO: Cholesky ?
  
  # Sample data.
  data <- MASS::mvrnorm(n, rep(0, d), sigma)

  # Scale to unit-variance.
  data <- apply(
    data,
    2,
    function(e) {
      vr <- var(e)
      sapply(
        e,
        function(o) {
          o / sqrt(vr)
        })
      })
  # Get empirical cov-matrix
  sigmahat <- cor(data)

  # Organize return to align with `huge`
  ret_list <- list(
    data = data,
    sigma = sigma,
    sigmahat = sigmahat,
    theta = theta
  )
  class(ret_list) <- "sim"
  return(ret_list)
}
