rm(list=ls())
library(BSDA)
library(car)
### ANOVA: Used to compare more than two populations

data(Groupabc)

### There are three groups A,B,C
### Goal: Compare if there are differences in means

### H_0: mu_A = mu_B = mu_C
### H_1: At least one of the means are different

### Test for Assumptions: Equal variance, samples are normally distributed


### Visualization: Histogram

X_A<-subset(Groupabc,group=="A")
X_B<-subset(Groupabc,group=="B")
X_C<-subset(Groupabc,group=="C")
plot(density(X_A$response,lwd=2,col=rgb(0,0.52,0.52),xlab="Responses",ylab="Density",main="Sample Comparison"),xlim=c(100,200))
lines(density(X_B$response),lwd=2,col="tomato")
lines(density(X_C$response,na.rm = T),lwd=2,col="royalblue")

### Conclusion: From visualization, data is normal, equal variances

### Now Shapiro Wilkes Test for normality

sh.a<-shapiro.test(X_A$response)
sh.b<-shapiro.test(X_B$response)
sh.c<-shapiro.test(X_C$response)

### Each group is normally distributed, since their p-values are > 0.05. Do not reject null => Data is normal

### Test for equal variances

lev.abc<-leveneTest(response~group,data=Groupabc)

### Levene's Test: H_0: Equal variances, H_1: Unequal variances
### p-value = 0.7726 > 0.05. Do not reject H_0
### Conclusion: Variances are equal


### ANOVA assumptions are satisfied



### ANOVA test using aov

aov.g<-aov(response~.,data=Groupabc)
summary(aov.g)

### Conclusion: p-value = 0.212 > 0.05
### Do not reject H_0. The means for all three groups are statistically the same
### Is a Post hoc Comparison required? No. Only use if ANOVA concluded one of the means is different




### ANOVA test using anova()

anova.g<-anova(lm(response~.,data=Groupabc))
print(anova.g)


