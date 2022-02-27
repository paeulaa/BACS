#1a.
d1 <- rnorm(n=500, mean=45, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=15, sd=5)
d123 <- c(d1, d2, d3)
plot(density(d123), col="blue", lwd=2, main = "Distribution 2")
abline(v=mean(d123), lwd = 2)
abline(v=median(d123))
mean(d123)
median(d123)
#1b
d1_ <- rnorm(n=800, mean=30, sd=5)
plot(density(d1_), col="blue", lwd=2, main = "Distribution 3")
abline(v=mean(d1_), lwd = 2)
abline(v=median(d1_))
mean(d1_)
median(d1_)
#1c
#Means are more sensitive,because they are more susceptible to extreme values than medians.

#2a
rdata <- rnorm(n=2000, mean=0, sd=1)
plot(density(rdata), col="blue", lwd=2, main = "random dataset")
abline(v=mean(rdata), lwd = 2)
abline(v=mean(rdata)-sd(rdata), lty="dashed")
abline(v=mean(rdata)+sd(rdata), lty="dashed")
abline(v=mean(rdata)-2*sd(rdata), lty="dashed")
abline(v=mean(rdata)+2*sd(rdata), lty="dashed")
abline(v=mean(rdata)-3*sd(rdata), lty="dashed")
abline(v=mean(rdata)+3*sd(rdata), lty="dashed")

#2b
quartiles = quantile(rdata)
#(1)
quartiles[2:4]
#(2)
(quartiles[2:4]-mean(rdata))/sd(rdata)

#2c
new_rdata <- rnorm(n=2000, mean=35, sd=3.5)
new_quartiles = quantile(new_rdata)
(new_quartiles[2]-mean(new_rdata))/sd(new_rdata)
(new_quartiles[4]-mean(new_rdata))/sd(new_rdata)

#Compared to (b), the amount of standard deviation away from the mean in (c) is almost the same with (b)'s.
#They are standard score of their normal distribution.

#2d
d1 <- rnorm(n=500, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=45, sd=5)

d123 <- c(d1, d2, d3)
d123_quartiles = quantile(d123)
(d123_quartiles[2]-mean(d123))/sd(d123)
(d123_quartiles[4]-mean(d123))/sd(d123)

#The result are the similar with answer (c). 
#Compared to (b), the amount of standard deviation away from the mean in (d) is almost the same with (b)'s.
#They are standard score of their normal distribution.

#3a
#h=2*IQR(x)/n^(1/3)
#which is based on the interquartile range, denoted by IQR. 
#It replaces 3.5σ of Scott's rule with 2 IQR, 
#which is less sensitive than the standard deviation to outliers in data.

#3b
rand_data <- rnorm(800, mean=20, sd = 5) 
#Sturges' formula
k_1 <- ceiling(log2(length(rand_data))+1)
h_1 <- (max(rand_data) - min(rand_data))/k_1
k_1
h_1
#Scott’s normal reference rule
h_2 <- 3.49*sd(rand_data)/(length(rand_data)^(1/3))
k_2 <- ceiling((max(rand_data) - min(rand_data))/h_2)
k_2
h_2
#Freedman-Diaconis’ choice
h_3 <- 2*IQR(rand_data)/(length(rand_data)^(1/3))
k_3 <- ceiling((max(rand_data) - min(rand_data))/h_3)
k_3
h_3

#3c
out_data <- c(rand_data, runif(10, min=40, max=60))
#(a)
k1 <- ceiling(log2(length(data))+1)
h1 <- (max(out_data) - min(out_data))/k1
#(b)
h2 <- 3.49*sd(out_data)/(length(out_data)^(1/3))
#(c)
h3 <- 2*IQR(out_data)/(length(out_data)^(1/3))
c(h_1, h_2, h_3)
c(h1, h2, h3)

#Freedman-Diaconis’ choice
# Because it is based on interquartile range, and make the result less sensitive to outer data than simple standard deviation. 

