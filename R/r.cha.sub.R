r.cha.sub <-
function(x.sub, x, weight.sub, weight, k, beta){
  n <- length(x)
  if(is.null(weight)){
    weight <- rep(1, n)
    weight.sub <- rep(1, length(x.sub))
  }
  rhow <- k*weighted.median(x, weight)
  ind <- ifelse(x.sub > rhow, 1, 0)
  r.cha <- sum((1-(rhow/(x.sub))^beta*ind)*weight.sub)/sum(weight.sub)
  return(r.cha)
}
