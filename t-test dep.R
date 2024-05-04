### t-test for dependent means
### t-test for correlated means, matched pair t-test
### t-test: One sample case (previously) H_0: mu=0 ; H_1: mu NOT =0
rm(list=ls())
library(BSDA)
### Data: Chips (From BSDA)
data(Chips)
### Consider Wafer 1: Repeated Measures Design
### Subjected to 4 measurements
### Does the average thickness of oxide layer differ between measurement 1 and 2 for Wafer 1
### For Wafer 1
boxplot(Chips$wafer11, Chips$wafer12, data=Chips, col=c("tomato", "turquoise"), main="Oxide Layer Thickness")
### mu_d = difference in average distance
### Consistent Oxide Level Thickness in Wafer 1 => H_0: mu_d = 0 (No difference in thickness); H_1: mu_d != 0 (Differences in thickness)
### Difference = First Measurement - Second Measurement
d <- Chips$wafer11-Chips$wafer12
t.test(d)
dep.t <- t.test(Chips$wafer11, Chips$wafer12, alternative = "two.sided", paired = T)
print(dep.t)
### Test Statistic: 1.0523, degrees of freedom: (n-1) = 29, p-value: 0.3013
### Conf.Level: 0.95, sig.level(alpha): (1-0.95) = 0.05
### Max Prob(Type 1 error) at which to reject H_0: 0.05
### Min Prob(Type 1 error) at which to reject H_0: 0.3013
### Conclusion: Since p-value > 0.05, do NOT reject H_0
### No difference in measurements 1 and 2 (consistent)
