rm(list=ls())
library(BSDA)
library(car)

### ANOVA: Using Irises data

data(Irises)

X_a<-subset(Irises, species =="setosa")
X_b<-subset(Irises, species =="versicolor")
X_c<-subset(Irises, species =="virginica")

iris.df<-data.frame(setosa=X_a$petal_length, versicolor=X_b$petal_length, virginica=X_c$petal_length)

### Density Curves

plot(density(X_a$petal_length,data=X_a), xlab="petal_length", lwd=2, col="purple", xlim=c(0,8))
lines(density(X_b$petal_length), lwd=2, col="pink", add=T)
lines(density(X_c$petal_length, lwd=2, col="forestgreen"))

### Goal: Are there differences in average petal_length based on species

### H_0: mu_setosa = mu_versicolor = mu_virginica
### H_1: At least one of the means are different



### Validate Assumptions: Normality

set.sh<-shapiro.test(iris.df$setosa)
print(set.sh)

ver.sh<-shapiro.test(iris.df$versicolor)
print(ver.sh)

vir.sh<-shapiro.test(iris.df$virginica)
print(vir.sh)

### Although setosa data is a borderline case with p-value = 0.05481, Assume normality for each set


### Validate Assumptions: Equal Variances

iris.lev<-leveneTest(Irises$petal_length~species, data =Irises, center=mean)

### p-value in Levene Test is extremely small. Variances are not equal
### Cannot use ANOVA



######### FOR SAKE OF ILLUSTRATION, ASSUME EACH ASSUMPTION IS SATISFIED: CONTINUE WITH ANOVA



iris.anv<-aov(petal_length~species, data=Irises)

### Print: ANOVA SUMMARY

summary(iris.anv)

### Conclusion: p-value is extremely small. REJECT H_0
### There are significant differences in average petal length among the three species

### However, Assumptions are violated, so ANOVA can't be used for this example



### Nonparametric Equivalent to ANOVA: Krusskal-Wallis Test

iris.kw<-kruskal.test(petal_length~species, data=Irises)
print(iris.kw)

### Conclusion: Same as ANOVA, reject H_0
### However, Kruskal-Wallis is the appropriate test here (Comparing medians/location parameters)

### Post hoc Comparisons
### Number of pairs: k(k-1)/2 = 3. => k=3 species

pair.set_vers<-t.test(iris.df$setosa, iris.df$versicolor, var.equal = F)
print(pair.set_vers)

pair.set_vir<-t.test(iris.df$setosa, iris.df$virginica, var.equal = F)
print(pair.set_vir)

pair.ver_vir<-t.test(iris.df$versicolor, iris.df$virginica, var.equal = F)
print(pair.ver_vir)

### Every pair of means is different because each pair's p-value is very small
