rm(list = ls())
set.seed(128)
### True Mean ###
known.mean<-16.25
known.sigma<-1
### Sample Size
sample.size<-128
x<-rnorm(sample.size, known.mean, sd=known.sigma)
hist(x)
print(t.test(x, mu=16.25, alternative = "greater"))

###Power Simulation###

alpha <- 0.05
### Null Mean = 6.5; known SD = 1