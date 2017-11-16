# Homework 2 Problem 2
# Part (a)
# Parameters
N<-100
M<-200
n<-10000
ex_ret=numeric(n)
strat_wgt<-2/N
eql_wgt<-1/N
for (i in 1:n)
{
  mat<-matrix((runif(N*M)>0.5)*0.02-0.01, nrow=N, ncol=M)
  keep<-(mat[,1]==0.01)
  ret_vec<-rowSums(mat)
  ex_ret[i]<-strat_wgt*sum(ret_vec[keep])-eql_wgt*sum(ret_vec)
}
mean(ex_ret)
sd(ex_ret)
alpha<-0.01
sigma<-0.01*sqrt((M-1)/N)
alpha 
sigma
# Part (b)
ex_ret1=numeric(n)
for (i in 1:n)
{
  mat<-matrix((runif(N*M)>0.5)*0.02-0.01, nrow=N, ncol=M)
  keep<-(mat[,1]==0.01)
  siz<-sum(keep)
  ret_vec<-rowSums(mat) 
  ex_ret1[i]<-(1/siz)*sum(ret_vec[keep])-eql_wgt*sum(ret_vec)
}
mean(ex_ret1)
sd(ex_ret1)
