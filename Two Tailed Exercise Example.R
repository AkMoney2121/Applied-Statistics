### Exercise Example: 30 Members
### Don't have before/after, but weight loss represents the change from before - after
### Before/After difference definition
### Use two-tailed test
exer.res<-t.test(Exercise$loss, mu = 0, alternative = "two.sided")
print(exer.res)
### H_0: mu_d = 0 (No difference in weight), H_1: mu_d != 0 (Difference in weight)
### Since p value < 0.05, Reject H_0
### So, there is a difference in weight from using the exercise program
### Note: alpha = Pr(Type 1 Error given H_0) = 0.05
### alpha_min = Pr(Type 1 error given data) = 0.009786
### Provided that the observations for variable loss is based on d = before - after
