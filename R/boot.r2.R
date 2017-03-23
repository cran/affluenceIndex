boot.r2 <-
function(x, k, alpha, nsim, boot.index=c("r.cha", "r.fgt")){
if (boot.index == "r.cha"){  
r.cha1 = r.cha(x, k, alpha)$r.cha

n <-length(x)
Rbb <-NULL 
for(i in 1:nsim){
  s <-sample(1:n,n,replace=T) 
  ss <-x[s]
  rownames(ss) <-NULL

  R <-r.cha(ss, k, alpha)
  Rb <-R$r.cha
  Rbb=rbind(Rb,Rbb)
 # print(paste("Iteration:",i))
}
gamma <-0.95    

r.se <-sd(Rbb)
ci1_up = r.cha1 + qnorm(gamma)*r.se
ci1_low = r.cha1 - qnorm(gamma)*r.se
ci2_up = quantile(Rbb,1-(1-gamma)/2)
ci2_low = quantile(Rbb,(1-gamma)/2)

tab <-rbind(c(ci1_low, r.cha1, ci1_up), c(ci2_low, r.cha1, ci2_up))
colnames(tab) <-c("ci.low", "r.cha", "ci.up")
rownames(tab) <-c("norm", "quantile")
outlist=list(se.r.cha=r.se, summary=tab)
}

if (boot.index == "r.fgt"){  
r.fgt1 = r.fgt(x, k, alpha)$r.fgt

n <-length(x)
Rbb <-NULL 
for(i in 1:nsim){
  s <-sample(1:n,n,replace=T) 
  ss <-x[s]
  rownames(ss) <-NULL

  R <-r.fgt(ss, k, alpha)
  Rb <-R$r.fgt
  Rbb=rbind(Rb,Rbb)
 # print(paste("Iteration:",i))
}
gamma <-0.95    

r.se <-sd(Rbb)
ci1_up = r.fgt1 + qnorm(gamma)*r.se
ci1_low = r.fgt1 - qnorm(gamma)*r.se
ci2_up = quantile(Rbb,1-(1-gamma)/2)
ci2_low = quantile(Rbb,(1-gamma)/2)

tab <-rbind(c(ci1_low, r.fgt1, ci1_up), c(ci2_low, r.fgt1, ci2_up))
colnames(tab) <-c("ci.low", "r.fgt", "ci.up")
rownames(tab) <-c("norm", "quantile")
outlist=list(se.r.ftg=r.se, summary=tab)
}

return(outlist)
}
