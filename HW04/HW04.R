#first
pnorm(-3.7)
2200000*pnorm(-3.7)
#second
verizon <- read.csv("verizon.csv")
verizon
time <- verizon$Time
group <- verizon$Group
#i. Visualize the distribution of Verizon’s repair times, marking the mean with a vertical line
plot(density(time), lwd = 2, col = "blue", main = "Distribution")
mean_t <- mean(time)
abline(v= mean_t, lwd = 2, col = "green")
#ii. Given what PUC wishes to test, how would you write the hypothesis? (not graded)
# My hypothesis is 7.6 minutes.
#iii. Estimate the population mean, and the 99% confidence interval (CI) of this estimate
population_mean <- t.test(time, conf.level = 0.99)
population_mean
#iv. Using the traditional statistical testing methods we saw in class, find the t-statistic and p-value of the test
hyp <- 7.6
sample_size <- length(time)
sample_mean <- mean(time)
sample_sd <- sd(time)
se <- (sample_sd/sqrt(sample_size))
se
t <- (sample_mean - hyp)/se
t
df <- sample_size - 1
p <- 1 -pt(t,df)
p
#v. Briefly describe how these values relate to the Null distribution of t (not graded)
#
#For each test, the t-value is a way to quantify the difference between the population 
#means and the p-value is the probability of obtaining a t-value with an absolute value 
#at least as large as the one we actually observed in the sample data if the null hypothesis 
#is actually true.
#vi. What is your conclusion about the advertising claim from this t-statistic, and why?
#narrative


#third
#sample0 = sample(time, sample_size)
#i. Bootstrapped Percentile: Estimate the bootstrapped 99% CI of the mean
compute_sample_mean <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  mean(resample)
}
compute_sample_mean(time)
sample_means <- replicate(2000, compute_sample_mean(time))
per_ci_99 <- quantile(sample_means, probs=c(0.005, 0.995))
per_ci_99
#ii. What is the 99% CI of the bootstrapped difference between the population mean and the hypothesized mean?
boot_mean_diffs <- function(smaple0, hyp) { #mean_hyp??
  resample <- sample(smaple0, length(sample0), replace = TRUE)
  return(mean(resample) - hyp)
}
set.seed(42379878)
num_boots <- 2000
mean_diffs <- replicate(
  num_boots,
  boot_mean_diffs(time, hyp)
)
diff_ci_99 <- quantile(mean_diffs, probs=c(0.005, 0.995))
diff_ci_99
#iii. Bootstrapped t-Interval: What is 99% CI of the bootstrapped t-statistic?
boot_t_stat <- function(sample0, hyp) {
    resample <- sample(sample0, length(sample0), replace=TRUE)
    diff <- mean(resample) - hyp
    se <- sd(resample)/sqrt(length(resample))
    return(diff/se)
}
set.seed(2346786)
num_boots <- 2000
t_boots <-replicate(num_boots, boot_t_stat(time, hyp))
mean(t_boots)
t_ci_99 <- quantile(t_boots, probs = c(0.005, 0.995))
t_ci_99 
#iv. Plot separate distributions of all three bootstraps above
plot(density(sample_means), lwd = 2, col = "skyblue", main = "bootstraps percentile")
abline(v=per_ci_99, lty="dashed")

plot(density(mean_diffs),lwd = 2, col="skyblue", main = "bootstraps difference of means")
abline(v=diff_ci_99, lty="dashed")

plot(density(t_boots),lwd = 2, col="skyblue", main = "bootstraps t-interval")
abline(v=t_ci_99, lty="dashed")






