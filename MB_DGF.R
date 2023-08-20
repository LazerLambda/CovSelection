

#' `mb_gen` Function
#' 
#' Reproduce data-generating process from Meinshausen and Bühlmann (2006).
#' @param n number of observations.
#' @param d number of variables.
#' @param trnct flag to keep column of selection.
#' @return list object including the generated data.
mb_gen <- function(n, d, trnct = TRUE) {
  # Sample from two-dimensional std. uniform dist.
  init_set <- data.frame(x = runif(n), y = runif(n))
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
  return(df)
  
  # TODO: Select nodes that have more than 4 (set var) neighbors
  # TODO: Remove edges randomly from set of non-conforming nodes
  # TODO: Repeat until condition of max neighbors is met.
}