# Import data and select right dates
ff3<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk3\\F-F_Research_Data_Factors_CSV\\F-F_Research_Data_Factors.csv', header=TRUE)
p25<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk3\\25_Portfolios_5x5_CSV\\25_Portfolios_5x5.csv', header=TRUE)
t1f=(ff3$Month>=199101 & ff3$Month<=201709)
t2f=(ff3$Month>=199101 & ff3$Month<=200512)
t3f=(ff3$Month>=200601 & ff3$Month<=201709)
# Part (a)
sharpa1=mean(ff3[t1f, 4])/sd(ff3[t1f, 4])
sharpa2=mean(ff3[t2f, 4])/sd(ff3[t2f, 4])
sharpa3=mean(ff3[t3f, 4])/sd(ff3[t3f, 4])
sharpa1
sharpa2
sharpa3
va1=c(1)
va2=c(1)
va3=c(1)
for (i in ff3[t1f, 4]/100)
{
  va1=c(va1, va1[length(va1)]*(1+i))
}
for (i in ff3[t2f, 4]/100)
{
  va2=c(va2, va2[length(va2)]*(1+i))
}
for (i in ff3[t3f, 4]/100)
{
  va3=c(va3, va3[length(va3)]*(1+i))
}
plot(va1, main='Wealth Path 199101-201710', xlab="Month", ylab='US$', type='l')
plot(va2, main='Wealth Path 199101-200512', xlab="Month", ylab='US$', type='l')
plot(va3, main='Wealth Path 200601-201710', xlab="Month", ylab='US$', type='l')
# Part (b)
sharpb1=mean(rowMeans(p25[t1f, 2:26]))/sd(rowMeans(p25[t1f, 2:26]))
sharpb2=mean(rowMeans(p25[t2f, 2:26]))/sd(rowMeans(p25[t2f, 2:26]))
sharpb3=mean(rowMeans(p25[t3f, 2:26]))/sd(rowMeans(p25[t3f, 2:26]))
sharpb1
sharpb2
sharpb3
vb1=c(1)
vb2=c(1)
vb3=c(1)
for (i in rowMeans(p25[t1f, 2:26])/100)
{
  vb1=c(vb1, vb1[length(vb1)]*(1+i))
}
for (i in rowMeans(p25[t2f, 2:26])/100)
{
  vb2=c(vb2, vb2[length(vb2)]*(1+i))
}
for (i in rowMeans(p25[t3f, 2:26])/100)
{
  vb3=c(vb3, vb3[length(vb3)]*(1+i))
}
plot(vb1, main='Wealth Path 199101-201710', xlab="Month", ylab='US$', type='l')
plot(vb2, main='Wealth Path 199101-200512', xlab="Month", ylab='US$', type='l')
plot(vb3, main='Wealth Path 200601-201710', xlab="Month", ylab='US$', type='l')
# Part (c)
usecol<-c(6,11,16,21,26)
sharpc1=mean(rowMeans(p25[t1f, usecol]))/sd(rowMeans(p25[t1f, usecol]))
sharpc2=mean(rowMeans(p25[t2f, usecol]))/sd(rowMeans(p25[t2f, usecol]))
sharpc3=mean(rowMeans(p25[t3f, usecol]))/sd(rowMeans(p25[t3f, usecol]))
sharpc1
sharpc2
sharpc3
vc1=c(1)
vc2=c(1)
vc3=c(1)
for (i in rowMeans(p25[t1f, usecol])/100)
{
  vc1=c(vc1, vc1[length(vc1)]*(1+i))
}
for (i in rowMeans(p25[t2f, usecol])/100)
{
  vc2=c(vc2, vc2[length(vc2)]*(1+i))
}
for (i in rowMeans(p25[t3f, usecol])/100)
{
  vc3=c(vc3, vc3[length(vc3)]*(1+i))
}
plot(vc1, main='Wealth Path 199101-201710', xlab="Month", ylab='US$', type='l')
plot(vc2, main='Wealth Path 199101-200512', xlab="Month", ylab='US$', type='l')
plot(vc3, main='Wealth Path 200601-201710', xlab="Month", ylab='US$', type='l')
# Part (d)
p25<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk3\\25_Portfolios_5x5_CSV\\25_Portfolios_5x5.csv', header=TRUE)
# b=which(p25$Month==199101)
b=which(p25$Month==200601)
e=which(p25$Month==201709)
# e=which(p25$Month==200512)
opt_ret=c()
for (i in b:e)
{
  sharpe=c()
  for (j in 2:26)
  {
    s=mean(p25[1:(i-1), j])/sd(p25[1:(i-1), j])
    sharpe=c(sharpe, s)
  }
  idx=which.max(sharpe)
  opt_ret=c(opt_ret, p25[i, (1+idx)]/100)
}
sharp=mean(opt_ret)/sd(opt_ret)
sharp
vd=c(1)
for (i in 1:length(opt_ret))
{
  vd=c(vd, vd[length(vd)]*(1+opt_ret[i]))
}
plot(vd, main='Wealth Path 200601-201710', xlab="Month", ylab='US$', type='l')

