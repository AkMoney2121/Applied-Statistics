### Using Darwin data from BSDA

data(Darwin)
cross<-Darwin$cross
self<-Darwin$self

### M_C = Median height of cross fertilized plants
### M_F = Median height of self fertilized plants

### H_0: M_C equal to M_F
### H_1: M_C not equal to M_F


### Testing for normality using Shapiro Wilkes Test

cross.sh<-shapiro.test(cross)
self.sh<-shapiro.test(self)

### Cross sample is NOT normally distributed because p-value = 0.0009744 < 0.05
### Self sample is normally distributed because p-value = 0.3771 > 0.05

### If both samples are normal, use 2 sample t-test
###Since one sample is not normal, Wilcoxon Rank Sum test



### Wilcoxon Rank Sum

plant.wc<-wilcox.test(cross, self, paired = F, alternative = "two.sided")

### Since p-value = 0.002608 < 0.05
### We reject H_0 
### There is sufficient evidence to conclude that the median height of cross fertilized plants is not equal to the median height of self fertilized plants