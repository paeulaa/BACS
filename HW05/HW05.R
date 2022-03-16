#(question1)
#   i. Would this scenario create systematic or random error (or both or neither)?
#  ii. Which part of the t-statistic or significance (diff, sd, n, alpha) would be affected?
# iii. Will it increase or decrease our power to reject the null hypothesis?
#  iv. Which kind of error (Type I or Type II) becomes more likely because of this scenario?
#a
#i.   systematic error
#ii.  n will be effected, because we should gain more sampls to make our measurement representitive.
#iii. Decrease the power to reject null hypothesis.
#iv.  TypeII error

#b
#i.   random error
#ii.  n will be effected. We will get less data to make our result correct.
#iii. Increase the power to reject null hypothesis.
#iv.  TypeI error

#c
#i.   systematic error
#ii.  It will affect the confidence level of our measurement.
#iii. Increase the power to reject null hypothesis.(increse the number of power test)
#iv.  TypeI error

#d
#i.   systematic error
#ii.  It will increase the mean and diff of smartphone usage.
#iii. Increase the power to reject null hypothesis.
#iv.  TypeI error

#(question2)
verizon <- read.csv("verizon.csv")
time <- verizon$Time
#a(i)
t.test(time, mu=7.6, alternative="greater", conf.level=0.99)#one tailed mean greater or less???
#a(ii)
hyp <- 7.6
power.t.test(n = length(time), delta = mean(time) - hyp, sd = sd(time), alternative = "one.sided")

#b(i)
se <- sd(time) / sqrt(length(time))
t_value <- (mean(time) - hyp)/se
t_value

#b(ii)
bootstrap_null_alt <- function(sample0, hyp_mean) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  resample_se <- sd(resample) / sqrt(length(resample))
  t_stat_alt <- (mean(resample) - hyp_mean) / resample_se
  t_stat_null <- (mean(resample) - mean(sample0)) / resample_se
  c(t_stat_alt, t_stat_null)
}
boot_t_stats <- replicate(10000, bootstrap_null_alt(time, hyp))
t_alt <- boot_t_stats[1,]
t_null <- boot_t_stats[2,]
plot(density(t_alt), xlim=c(-4,6), col="skyblue", lwd = 2)
lines(density(t_null), lty = "dashed", col = "green")

#b(iii)
cutoff_99 <-quantile(t_null, probs=c(0.005, 0.995))
cutoff_99
# cutoff_95 <-quantile(t_null, probs=c(0.025, 0.975))
# cutoff_95
abline(v=t_value, col = "red")
abline(v=cutoff_99, lty ="dotted")

#b(iv)
null_probs <- ecdf(t_null)
one_tailed_pvalue <- 1 - null_probs(t_value)
one_tailed_pvalue

cutoff_99 <-quantile(t_null, probs=c(0.005, 0.995))
alt_probs <- ecdf(t_alt)
alt_probs(cutoff_99[1])+ (1 - alt_probs(cutoff_99[2]))



