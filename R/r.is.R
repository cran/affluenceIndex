r.is <-
function(x,p){
q.p <-rep(quantile(x,c(p)), length(x))
ind <- ifelse(x>q.p,1,0)
r.2 <-sum(x*ind)/sum(x)

return(r.2)
}
