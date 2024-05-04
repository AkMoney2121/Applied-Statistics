rm(list=ls())
library(BSDA)
data(Alcohol)
## Ages at which 14 female alcoholics began drinking
X<-Alcohol
## H_0: Average age is <= 21, H_1: Average age is > 21
test.result<-t.test(X, mu=21, alternative="greater")
print(test.result)
## Conclusion: Do not reject null (p-value = 0.5925 > .05).
## Average age is NOT > 21
##is two tail test necessary? No, but we'll do it anyway
## H_0: average age is >= 21, H_1: Average age is < 21
## Left tail test
test.result.1<-t.test(X,mu=21,alternative="less",)
print(test.result.1)
##Right tail test
test.result.1<-t.test(X,mu=21,alternative="greater",)
print(test.result.1)
##Two tail test
test.result.1<-t.test(X,mu=21,alternative="two.sided",)
print(test.result.1)
##Critical value using t dist with df=13 (null dist)
##Prob below cutoff (critical value) for given alpha = 0.05
alpha<-0.05
cutoff.cv<-qt(alpha, 13, lower.tail = F)
x<-seq(-3,3,length=128)
plot(x,dt(x,13), type="1")
points(-.23876,0,pch=19,col="blue")