rm(list=ls())
require(BSDA)
data(Censored)
cens<-Censored
cens
##########################

#Finding sample for first treatment (A)
trt.A<-subset(cens,treatment=="A")

#Finding sample for second treatment (B)
trt.B<-subset(cens,treatment=="B")

####Compare Survival Times (in days) among the treatments
#Summary Statistics(5-number summary): Treatment A
fv.A<-fivenum(trt.A$survival)
print(fv.A)
sd.A<-sd(trt.A$survival)
var.A<-(sd.A)^2

#Summary Statistics(5-number summary): Treatment B
sm.B<-summary(trt.B$survival)
print(sm.B)
sd.B<-sd(trt.B$survival)
var.B<-(sd.B)^2

####Hypothesis: Average survival time of Treatment A is > Treatment B

#Boxplot for treatments A,B
boxplot(survival~treatment, data=cens)

##From boxplot, Treatment A has greater variability than Treatment B

#Histograms
hist(trt.A$survival,angle=45,col=4, xlim=c(80,2000))
hist(trt.B$survival,angle=45,col=rgb(255,99,71,150,maxColorValue=255),add=TRUE)

#Density Curves
den.A<-density(trt.A$survival)
den.B<-density(trt.B$survival)
plot(den.A, col="royalblue", lwd=2, xlim=c(80,2000))
lines(den.B,col="turquoise")

####Violated Assumptions: Graphs are not normally distributed
#### Case: Unequal Variances ####


##First testing as if variances are equal to show what the results are like

#H_0: Avg survival time of Treatment A (mu_A) = Avg survival time of Treatment B (mu_B)
#H_1: Survival times are not equal

surv.test.eq<-t.test(trt.A$survival, trt.B$survival, alternative = "two.sided", var.equal = T)
surv.test.eq

### Inference 1: 0 is not contained in the 95% confidence interval. It's unlikely that the means are equal
### Inference 2: (P-Value) Minimum value at which we reject H_0 = 0.005388. p-value is less than critical value (0.05)
### Estimated Pr(Type 1 error | Data) = 0.005388
#### Conclusion: mu_A is NOT equal to mu_B


###Now, Testing as if variances are unequal

surv.test.uneq<-t.test(trt.A$survival, trt.B$survival, alternative = "two.sided", var.equal = F)
surv.test.uneq

#### Even though the conclusions did not vary, the correct procedure is Welch's T-Test because of unequal variances

######## Regardless, our conclusions are unreliable because the data is not normally distributed #######
### We will remove outliers to achieve normality

trt.A.rm<-trt.A$survival[trt.A$survival<=1200]
trt.B.rm<-trt.B$survival[trt.B$survival<=1200]

var(trt.A.rm)
var(trt.B.rm)

plot(density(trt.A.rm), lwd=2, col=rgb(0,128,128,maxColorValue = 225))
lines(density(trt.B.rm), lwd=2, col="turquoise")

#### Still doesn't look normal

#### Shapiro-Wilkes test for Normality
# H_0: Data is normal. H_1: Data is not normal

sh.trt.A<-shapiro.test(trt.A.rm)
sh.trt.A
sh.trt.B<-shapiro.test(trt.B.rm)
sh.trt.B

#### Reject H_0 for both cases (P-value is less than significance level of 0.05. Therefore, the data is not normally distributed


surv.test.uneq.lefttail<-t.test(trt.A$survival, trt.B$survival, alternative = "less", var.equal = F)
surv.test.uneq.righttail<-t.test(trt.A$survival, trt.B$survival, alternative = "greater", var.equal = F)
