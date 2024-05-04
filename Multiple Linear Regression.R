rm(list=ls())
library(BSDA)
library(car)

data(iris)

### Multiple Linear Regression


df<-subset(iris,Species=="setosa")

### Y = Petal Width (Response)
### X1 = Petal Length, X2 = Sepal Length, X3 = Sepal Width

### Y = b_0 + b_1*X_1 + b_2*X_2 + b_3*X_3 + epsilon
### Assumption: epsilon (noise) is normally distributed

mod.iris<-lm(Petal.Width~Petal.Length+Sepal.Length+Sepal.Width, data=df)

### Coefficient Summary
print(summary(mod.iris))

### Intercept in Summary = beta_0
### Estimates show coefficients


### Fitted Model

### Y_hat=-0.294 + 0.1690*X_1 + 0.0454*X_2 + 0.019*X_3 (FULL MODEL)
### Is petal width dependent on petal length, sepal length, and sepal width at alpha = 0.1?
### NO, because the coefficients for sepal length/width are not significant
### Conclusion: There is a relationship between Petal Length and Petal Width, but not sepal length/width (For Setosa)

mod.iris.red<-lm(Petal.Width~Petal.Length, data = df)
print(summary(mod.iris.red))

### Y_hat_reduced = -0.0408 + 0.203*X_1 (Reduced Model)
### Just because two variables are not significant and you drop them, you can't just use beta_0 and beta_1
### You must run a reduced model and find new coefficients for significant dependent variables



### Full Model ANOVA

anv.mod<-anova(mod.iris)
print(anv.mod)

### Reduced Model ANOVA

anv.mod.red<-anova(mod.iris.red)
print(anv.mod.red)

### R-Squared is not too high, therefore neither model is great at explaining Petal Width using Petal Length
### Petal length is statistically sigificant based on p-values, but not practically significant based on R squared


### Check normality by plot

plot(mod.iris$residuals, pch=19, col=4, main="Residual Plot")
abline(h=0, lwd=2, col=2)


### Shapiro Test

print(shapiro.test(mod.iris$residuals))

### Normality assumption is violated
### p-value is way less than 0.05
### Residuals are not normal

### Check for reduced model

plot(mod.iris.red$residuals, pch=19, col=4, main="Residual Plot")
abline(h=0, lwd=2, col=2)

### Shapiro Test

print(shapiro.test(mod.iris.red$residuals))

### P-value way less than 0.05
### Residuals are not normal for reduced case as well

### Conclusion: Linear model is not a good option

scatterplotMatrix(df)


