#1.a
#I choose tidyr, because tidy output only var and value. However, reshape2 also output id element. 
#In this question, we don't need id, so the formation od tidyr is more tidy to me.

#1.b
data <- read.csv("./verizon_wide.csv")
data
# ILEC <- na.omit(data$ILEC)
# CLEC <- na.omit(data$CLEC)
# long <- data.frame(
#   load_time= c(ILEC, CLEC),
#   host = c(rep("ILEC", length(ILEC)), rep("CLEC", length(CLEC)))
# )
# loads_long <- melt(data, na.rm = TRUE, variable.name = "host", value.name = "load_time")
# loads_long
library(tidyr)
load_long <- gather(data, na.rm = TRUE, key = "host", value = "load_time")
load_long
library(reshape2)
loads_long <- melt(page_loads, na.rm = TRUE, variable.name = "host", value.name = "load_time")
loads_long
#1.c
head(long)
tail(long)

#1.d
hosts <- split(x = long$load_time, f = long$host)
plot(density(hosts$ILEC), col = "red", lwd = 2)
lines(density(hosts$CLEC), col = "green", lwd = 2)

#2.a
#H0:ILEC = CLEC
#H1:ILEC > CLEC 

#2.b
#i
t.test(hosts$ILEC, hosts$CLEC, conf.level = 0.99, alt = "two.sided", var.equal = TRUE)
#ii
t.test(hosts$ILEC, hosts$CLEC, conf.level = 0.99, alt = "two.sided", var.equal = FALSE)
#not reject

#2.c
#i
ob_diff <- mean(hosts$ILEC) - mean(hosts$CLEC)
permute_diff <- function(values, groups){
  permuted <- sample(values, replace = FALSE)
  grouped <- split(permuted, groups)
  permute_diff <- mean(grouped$ILEC) - mean(grouped$CLEC)
}
nperms <- 10000
permute_diffs <- replicate(nperms, permute_diff(long$load_time, long$host))
hist(permute_diffs, col="grey", breaks = "fd", probability = TRUE)
lines(density(permute_diffs), lwd = 2)
abline(v=ob_diff, col="darkred", lwd = 2)
#ii
p_1tailed <- sum(permute_diffs > ob_diff) / nperms
p_1tailed

p_2tailed <- sum(abs(permute_diffs) > ob_diff) / nperms
p_2tailed
#iii 
t.test(hosts$ILEC, hosts$CLEC, conf.level = 0.99, alt = "greater", var.equal = TRUE)
t.test(hosts$ILEC, hosts$CLEC, conf.level = 0.99, alt = "greater", var.equal = FALSE)
#Both p-values 0.9701 and 0.9955 are bigger than significance level, so we should not reject H0.

#3.a
time_ranks <- rank(long$load_time)
ranked_groups <- split(time_ranks, long$host)
U1 <- sum(ranked_groups$ILEC)
n1 <- length(hosts$ILEC)
n2 <- length(hosts$CLEC)
W <- U1 - (n1*(n1+1))/2
W

#3.b
wilcox_p1tail <- 1 - pwilcox(W, n1, n2)
wilcox_p1tail
wilcox_p2tail <- 2 * wilcox_p1tail
wilcox_p2tail

#3.c
wilcox.test(hosts$ILEC, hosts$CLEC, alt = "greater")

#3.d
#Not reject, because p-value is greater than 0.01

#4.a
norm_qq_plot <- function(values){
  probs1000 <- seq(0, 1, 0.001)
  q_vals <- quantile(values, probs = probs1000)
  q_norm <- qnorm(probs1000, mean = mean(values), sd = sd(values))
  plot(q_norm, q_vals, xlab="normal quantiles", ylab="values quantiles")
  abline(a=0, b=1, col="red", lwd=2)
}

#4.b
set.seed(978234)
d1 <- rnorm(n=500, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=45, sd=5)
d123 <- c(d1, d2, d3)

plot(density(d123))
norm_qq_plot(d123)

#4.c
norm_qq_plot(hosts$ILEC)
norm_qq_plot(hosts$CLEC)
#In ILEC's plot, the upper tail is a little bit far away from red line, 
#so I think it is a "Skwed Right" histogram.On the other hand, CLEC's plot is normally distributed, 
#because of its symmetry.
