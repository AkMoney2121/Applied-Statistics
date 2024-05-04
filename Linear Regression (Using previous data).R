rm(list=ls())
library(BSDA)
library(car)

### ANOVA: Using Irises data

data(Irises)

X_a<-subset(Irises, species =="setosa")
X_b<-subset(Irises, species =="versicolor")
X_c<-subset(Irises, species =="virginica")

iris.df<-data.frame(setosa=X_a$petal_length, versicolor=X_b$petal_length, virginica=X_c$petal_length)


### Linear Models Assumption: Responses are normal
### Response: Petal length => Choices: Versicolor/Virginica  (CHOOSE VERSICOLOR)
### Process: Linear Regression
### Model: Petal Length (Y), Petal Width (X)

### Visualization: Determine if data is linear using scatterplot

plot(X_b$petal_width, X_b$petal_length, type = "p", cex=2, pch=19, col=4, xlab = "Petal Width", ylab = "Petal Length", main="Iris - Versicolor", xlim = c(0.6,2), ylim = c(2,6))
print(paste0("Linear Correlation Coefficient (r):", cor(X_b$petal_width, X_b$petal_length)))


### Linear Model
### Estimating petal_width = f(petal_length) *Function of petal length*
### Petal_width = beta_0 + beta_1 * (petal_length)  i.e) y=mx+b where petal_length is x

mod.iris<-lm(X_b$petal_length~X_b$petal_width)

### Print Model: Estimate for beta_0 = 1.781, Estimate for beta_1 = 1.869
### Fitted model: petal_width_hat = 1.781 + 1.869*(petal_length)

print(mod.iris)

### Significance of coefficients given by summary

print(summary(mod.iris))

### Estimates for intercept and slope are significant because p-value is < 0.05

lines(X_b$petal_width, mod.iris$fitted.values, col=1, lwd=2)

### Overall model significance

anv.iris<- anova(mod.iris)
print(anv.iris)

### Entire fitted model is significant because p-value is < 0.05. Overall fitted model is a good representation of the data
### Is it appropriate to estimate petal length of Iris (Versicolor) using its petal width with our model
### YES

### Check Assumptions

plot(mod.iris$residuals, pch=19, col=4, main="Residual Plot")
abline(h=0, lwd=2, col=2)

### Assumptions are NOT violated because the line is horizontal
### Alternatives: Shapiro Wilkes test for normality

iris.shp<-shapiro.test(mod.iris$residuals)
print(iris.shp)

### P-value > 0.05, do NOT reject H_0 => Normal

### Can also use QQ Plot to check normality


