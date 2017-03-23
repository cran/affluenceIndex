
r.hc <-function(x,k){
rho <-k*median(x)

r.3 <-length(x[x>rho])/length(x)

outlist=list(count.rich=length(x[x>rho]), r.hc=r.3)
return(outlist)	

}
