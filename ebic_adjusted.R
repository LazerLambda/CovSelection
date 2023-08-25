

ebic_corrected <- function(est,d, n, ebic.gamma = 0.5){
  if(est$method == "glasso")
  {
    est$ebic.score = -n*est$loglik + log(n)*est$df + 4*ebic.gamma*log(d)*est$df
    if (TRUE %in% (est$sparsity == 0)) {
      ind <- which(est$sparsity == 0)
      est$ebic.score[[ind]] <- max(est$ebic.score)
    }
    est$opt.index = which.min(est$ebic.score)
    est$refit = est$path[[est$opt.index]]
    est$opt.icov = est$icov[[est$opt.index]]
    if(est$cov.output)
      est$opt.cov = est$cov[[est$opt.index]]
    est$opt.lambda = est$lambda[est$opt.index]
    est$opt.sparsity = est$sparsity[est$opt.index]
  }
  est$criterion = "ebic"
  class(est) = "select"
  return(est)
}

