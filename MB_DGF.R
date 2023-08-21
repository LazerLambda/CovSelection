
library(MASS)

#' `mb_gen` Function
#' 
#' Reproduce data-generating process from Meinshausen and Bühlmann (2006).
#' @param n number of observations.
#' @param d number of variables.
#' @param trnct flag to keep column of selection.
#' @return list object including the generated data.
mb_gen <- function(n, d, trnct = TRUE) {
  # Sample from two-dimensional std. uniform dist.
  init_set <- data.frame(x = runif(d), y = runif(d))
  m <- as.matrix(dist(init_set))

  # Build set of all edges
  ln <- floor(nrow(m) * ncol(m) / 2) - ncol(m) / 2
  df <- data.frame(
    n1 = rep(NA, ln),
    n2 = rep(NA, ln),
    dist = rep(NA, ln))
  counter <- 1
  for(i in seq_len(nrow(m) - 1)) {
    for(j in seq_len(i)) {
      df$n1[counter] <- i + 1
      df$n2[counter] <- j
      df$dist[counter] <- m[i + 1, j]
      counter <- counter + 1
    }
  }

  # Get sample probability for edges
  df$prob <- dnorm(df$dist / sqrt(d))
  # Keep all nodes that have been chosen
  if(trnct){
    df <- df[as.logical(rbinom(ln, 1, df$prob)), ]
  } else {
    df$selection <- as.logical(rbinom(ln, 1, df$prob))
  }
  return(list(data = df))
  
  # TODO: Select nodes that have more than 4 (set var) neighbors
  # TODO: Remove edges randomly from set of non-conforming nodes
  # TODO: Repeat until condition of max neighbors is met.
}


edge_cond_met <- function(axis, m, upper) {
  (apply(m, axis, sum, na.rm=TRUE) > upper)
}

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

test_equalize_edges <- function(n = 100, upper = 4) {
  m <- matrix(sample(c(TRUE,FALSE), n^2, TRUE), nrow = n, ncol = n)
  m[upper.tri(m, diag = TRUE)] <- NA
  m <- equalize_edges(m, upper = upper)
  checkmate::assert(!(TRUE %in% (apply(m, 1, sum, na.rm = T) > upper)))
  checkmate::assert(!(TRUE %in% (apply(m, 2, sum, na.rm = T) > upper)))
  
  # Check upper triangle NA
}

#' `mb_gen_m` Function
#' 
#' Reproduce data-generating process from Meinshausen and Bühlmann (2006).
#' 
#' TODO: Explain in detail!
#' 
#' @param n number of observations.
#' @param d number of variables.
#' @param trnct flag to keep column of selection.
#' @return list object including the generated data.
mb_gen_m <- function(n, d, upper = 4) {
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
  
  m <- 1 * m
  theta <- duplicate(m, shallow = FALSE)
  m[upper.tri(m)] <- t(m[lower.tri(m)])
  m <- 0.245 * m
  diag(m) <- rep(1, d)
  sigma <- solve(m) # TODO: Cholesky ?
  data <- MASS::mvrnorm(n, rep(0, d), sigma)
  return(list(
    data = data,
    sigmahat = m,
    sigma = sigma,
    theta = NA
  ))
}



run_test <- function(){
  mb_gen(100, 500, T)
}