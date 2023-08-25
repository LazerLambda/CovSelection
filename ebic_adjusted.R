

ebic_corrected <- function(est,d, n, ebic.gamma = 0.5){
  if(est$method == "glasso")
  {
    # if (min(est$sparsity) == 0){
    #   indices <- which(!est$sparsity == 0)
    #   est$ebic.score = -n*est$loglik[indices] + log(n)*est$df[indices] + 4*ebic.gamma*log(d)*est$df[indices]
    #   est$opt.index = which.min(est$ebic.score[indices])
    # } else {
    #   
    #   est$opt.index = which.min(est$ebic.score[est$ebic.score != 0]) 
    # }
    est$ebic.score = -n*est$loglik + log(n)*est$df + 4*ebic.gamma*log(d)*est$df
    est$opt.index = which.min(est$ebic.score[est$ebic.score != n * d])
    print(est$loglik)
    print(n * d)
    print(est$ebic.score[est$ebic.score != n * d])
    print(est$opt.index)
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

