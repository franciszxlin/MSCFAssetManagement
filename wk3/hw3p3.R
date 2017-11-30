# Import data and select right dates
ff3<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk3\\F-F_Research_Data_Factors_CSV\\F-F_Research_Data_Factors.csv', header=TRUE)
p25<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk3\\25_Portfolios_5x5_CSV\\25_Portfolios_5x5.csv', header=TRUE)
# ff3<-ff3[ff3$Month>=196701,]
# p25<-p25[p25$Month>=196701,]
ff3<-ff3[ff3$Month>=198701,]
p25<-p25[p25$Month>=198701,]
# Part (a)
avg_ret=c()
se_ret=c()
for (i in 2:26)
{
  avg_ret<-c(avg_ret, mean(p25[,i]-ff3$RF))
  se_ret<-c(se_ret, sd(p25[,i]-ff3$RF))
}
avg_mat<-t(matrix(avg_ret, nrow=5))
se_mat<-t(matrix(se_ret, nrow=5))
t_mat<-t(avg_mat/se_mat)
avg_mat
se_mat
t_mat
# Part (c)
alphas<-c()
talphas<-c()
for (i in 2:26)
{
  fit<-lm((p25[,i]-ff3$RF)~ff3$Mkt.RF)
  output<-summary(fit)
  alphas<-c(alphas, fit$coefficients[1])
  talphas<-c(talphas, output$coefficients[1,3])
}
alpha_mat<-t(matrix(alphas, nrow=5))
talpha_mat<-t(matrix(talphas, nrow=5))
alpha_mat
talpha_mat
# Part (d)
as<-c()
tas<-c()
for (i in 2:26)
{
  fit<-lm((p25[,i]-ff3$RF)~ff3$Mkt.RF+ff3$SMB+ff3$HML)
  output<-summary(fit)
  as<-c(as, output$coefficients[1, 1])
  tas<-c(tas, output$coefficients[1, 3])
}
as_mat<-t(matrix(as, nrow=5))
tas_mat<-t(matrix(tas, nrow=5))
as_mat
tas_mat