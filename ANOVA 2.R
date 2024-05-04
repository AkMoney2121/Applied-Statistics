rm(list=ls())
library(BSDA)
library(car)
### ANOVA: Used to compare more than two populations

data(Hostile)

### Test for Assumptions

boxplot(hostility~location,data=Hostile,main="Hostility Comparison")

### Equal Variances?: Probably NOT
### Normally distributed?: Probably NOT

