#Problem 3
cur=read.table("C:\\Users\\zil20\\Desktop\\Asset Management\\wk5\\CurrencyReturnsClean.csv", header=TRUE, sep=",")
names(cur)[1]="date"
#Part (a)
for (i in 2:7)
{
  avg=12*mean(cur[,i])
  print(avg)
  vol=sqrt(12)*sd(cur[,i])
  print(vol)
  print(avg/vol)
  print(" ")
}
#Part (b)
for (i in 3:7)
{
  avg=12*mean(cur[,i]-cur[,2])
  print(avg)
  vol=sqrt(12)*sd(cur[,i]-cur[,2])
  print(vol)
  print(avg/vol)
  print(" ")
}
#Part (c)
cur$rx=(1/6)*(cur$Portfolio1+cur$Portfolio2+cur$Portfolio3+cur$Portfolio4+cur$Portfolio5+cur$Portfolio6)
cur$hml=cur$Portfolio6-cur$Portfolio1
r1=lm(Portfolio1~rx+hml, data=cur)
summary(r1)
r2=lm(Portfolio2~rx+hml, data=cur)
summary(r2)
r3=lm(Portfolio3~rx+hml, data=cur)
summary(r3)
r4=lm(Portfolio4~rx+hml, data=cur)
summary(r4)
r5=lm(Portfolio5~rx+hml, data=cur)
summary(r5)
r6=lm(Portfolio6~rx+hml, data=cur)
summary(r6)
