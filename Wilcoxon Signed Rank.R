rm(list = ls())
library(BSDA)

X<-c(2.2,3,3,4,5.1)

### Null: M_0 = 3.2

M_0=3.2

### 1-Sample Wilcoxon Test(Signed Rank)
### R gives you V+
### V = N(N+1)/2 - 2V-

wc<-wilcox.test(X,y=NULL,paired=F, mu=M_0, alternative = "less", correct = F)


### Data: Entrance
### Normality Assumption: Shapiro Wilkes Test
### BoxPlot

data("Entrance")
b<-boxplot(Entrance, main = "Entrance Exam Scores", xlab = "Scores", col = "royalblue")
sw<-shapiro.test(Entrance$score)

### Data is normal since p > 0.05, so we can use t-test or Wilcoxon Signed Rank


### Wilcoxon Signed Rank
### H_0: M<=65   H_1: M>65
### Right Tailed Test

wc<-wilcox.test(Entrance$score, conf.level = 0.95, alternative = "greater", mu=65)

### p-value = 0.02865 < 0.05
### Thus, reject H_0. Evidence suggests that the median entrance exam scores are above 65


### Let's check one sample t-test just in case (We can do this because data is normal)
### Note: Wilcoxon uses Median Scores, T.Test uses Mean(Average) Scores
### H_0: mu<=65   H_1: mu>65

et<-t.test(Entrance$score, conf.level = 0.95, alternative = "greater", mu=65)

### P value = 0.0273<0.05. 
### Thus, reject H_0. Evidence suggests that the mean entrance exam scores are above 65


