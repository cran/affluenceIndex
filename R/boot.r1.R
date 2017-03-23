boot.r1 <-function(x, k, nsim, boot.index=c("r.hc", "r.is")){
if (boot.index == "r.is"){  
r.is1 = r.is(x, k)	

n <-length(x)
Rbb <-NULL 
for(i in 1:nsim){
  s <-sample(1:n,n,replace=T) 
  ss <-x[s]
  rownames(ss) <-NULL

  R <-r.is(ss, k)
  Rb <-R
  Rbb=rbind(Rb,Rbb)
 # print(paste("Iteration:",i))
}
gamma <-0.95    

r.se <-sd(Rbb)
ci1_up = r.is1 + qnorm(gamma)*r.se
ci1_low = r.is1 - qnorm(gamma)*r.se
ci2_up = quantile(Rbb,1-(1-gamma)/2)
ci2_low = quantile(Rbb,(1-gamma)/2)

tab <-rbind(c(ci1_low, r.is1, ci1_up), c(ci2_low, r.is1, ci2_up))
colnames(tab) <-c("ci.low", "r.is", "ci.up")
rownames(tab) <-c("norm", "quantile")
outlist=list(se.r.is=r.se, summary=tab)
}

if (boot.index == "r.hc"){  
r.hc1 = r.hc(x, k)	

n <-length(x)
Rbb <-NULL 
for(i in 1:nsim){
  s <-sample(1:n,n,replace=T) 
  ss <-x[s]
  rownames(ss) <-NULL

  R <-r.hc(ss, k)
  Rb <-R$r.hc
  Rbb=rbind(Rb,Rbb)
 # print(paste("Iteration:",i))
}
gamma <-0.95    

r.se <-sd(Rbb)
ci1_up = r.hc1$r.hc + qnorm(gamma)*r.se
ci1_low = r.hc1$r.hc - qnorm(gamma)*r.se
ci2_up = quantile(Rbb,1-(1-gamma)/2)
ci2_low = quantile(Rbb,(1-gamma)/2)

tab <-rbind(c(ci1_low, r.hc1$r.hc, ci1_up), c(ci2_low, r.hc1$r.hc, ci2_up))
colnames(tab) <-c("ci.low", "r.hc", "ci.up")
rownames(tab) <-c("norm", "quantile")
outlist=list(se.r.hc=r.se, summary=tab)
}

return(outlist)
}
