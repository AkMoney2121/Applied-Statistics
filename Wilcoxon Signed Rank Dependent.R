rm(list = ls())
library(BSDA)

### Wilcoxon Signed Rank (Dependent Case)

### Using Fitness data

data(Fitness)
fit.b<-subset(Fitness,test == "Before")
fit.a<-subset(Fitness,test == "After")
fit.b<-fit.b[,3]
fit.a<-fit.a[,3]
fit.b<-as.numeric(unlist(fit.b))
fit.a<-as.numeric(unlist(fit.a))

### Objective: Test effectiveness of fitness course
### This is a Within Subjects test (Since we're testing effectiveness *before/after*)

### d = After - Before
### H_0: M_d = 0
### H_1: M_d not = 0
### difference(d) = X-Y in R

fit.wc<-wilcox.test(fit.a,fit.b,mu=0,paired = T)
print(fit.wc)

d<-fit.a - fit.b
fit.d<-wilcox.test(d,mu=0)
print(fit.d)

### p-value = 0.04688 < 0.05 => Reject H_0
### There is evidence to support H_1: that the physical fitness course has an effect in increasing the number of sit-ups

### Recommendation: Needs further investigation because p-value is very close to .05

### Remove the tie (When difference between fit.a and fit.b =0) to get exact p-value

d<-d[d!=0]
fit.we<-wilcox.test(d, mu=0)



### t-test results

d=fit.a-fit.b
fit.sh<-shapiro.test(d)

### p-value = 0.7037 => Do Not Reject
### Conclusion: Data is normal, t-test is appropriate

fit.tt<-t.test(d)

### p-value = 0.02494 < 0.05 => Reject H_0
### Conclusion: There is evidence to support H_1: that the physical fitness course has an effect in increasing the number of sit-ups
### 95% confidence interval for average difference: (0.3247, 3.6752)
