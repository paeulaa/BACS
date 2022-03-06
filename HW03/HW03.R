#1
#a)
#i)
standardized <-function(numbers) {
    std <-(numbers-mean(numbers)) / sd(numbers)
    return(std)
}
data <- rnorm(n = 10000, mean = 940, sd = 190)
rnorm_std <- standardized(data)
mean(rnorm_std)
sd(rnorm_std)
#It's mean will be 0, and sd will be 1, because it is a normal distribution.
#ii)
hist(rnorm_std, col="pink", main = "Distribution shape")
#It looks like a bell, because it is noraml distrbution.
#iii)
#Standard normal distibution, standard score, and z-score.
#b) 
bookings <- read.table("./first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
minday_std <- standardized(minday)
minday_std
mean(minday_std)
sd(minday_std)
#It's mean will not always be 0, because it's not a normal distribution.
hist(minday, col="pink", main = "minday")
hist(minday_std, col="skyblue", main = "minday_std")
#The shape of minday_std is more similar to bell shape than original minday.


#2
# Visualize the confidence intervals of samples drawn from a population
#   e.g.,
#     visualize_sample_ci(sample_size=300, distr_func=rnorm, mean=50, sd=10)
#     visualize_sample_ci(sample_size=300, distr_func=runif, min=17, max=35)
visualize_sample_ci <- function(num_samples = 100, sample_size = 100, pop_size=10000, distr_func=rnorm, ...) {
  # Simulate a large population
  population_data <- distr_func(pop_size, ...)
  pop_mean <- mean(population_data)
  pop_sd <- sd(population_data)
  
  # Simulate samples
  samples <- replicate(num_samples, sample(population_data, sample_size, replace=FALSE))
  
  # Calculate descriptives of samples
  sample_means = apply(samples, 2, FUN=mean)
  sample_stdevs = apply(samples, 2, FUN=sd)
  sample_stderrs <- sample_stdevs/sqrt(sample_size)
  ci95_low  <- sample_means - sample_stderrs*1.96
  ci95_high <- sample_means + sample_stderrs*1.96 
  ci99_low  <- sample_means - sample_stderrs*2.58
  ci99_high <- sample_means + sample_stderrs*2.58
  
  # Visualize confidence intervals of all samples
  plot(NULL, xlim=c(pop_mean-(pop_sd/2), pop_mean+(pop_sd/2)), 
       ylim=c(1,num_samples), ylab="Samples", xlab="Confidence Intervals")
  add_ci_segment(ci95_low, ci95_high, ci99_low, ci99_high, sample_means, 1:num_samples, good=TRUE)
  
  # Visualize samples with CIs that don't include population mean
  bad = which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) | ((ci99_low > pop_mean) | (ci99_high < pop_mean)))
  add_ci_segment(ci95_low[bad], ci95_high[bad], ci99_low[bad], ci99_high[bad], sample_means[bad], bad, good=FALSE)
  
  Not95 <- which(((ci95_low > pop_mean) | (ci95_high < pop_mean)))
  Not99 <- which(((ci99_low > pop_mean) | (ci99_high < pop_mean)))
  cat("Not in 95% ci:", length(Not95))
  cat("\nNot in 99% ci:", length(Not99))
  
  # Draw true population mean
  abline(v=mean(population_data))
}

add_ci_segment <- function(ci95_low, ci95_high, ci99_low, ci99_high, sample_means, indices, good=TRUE) {
  segment_colors <- list(c("lightcoral", "coral3", "coral4"), c("lightskyblue", "skyblue3", "skyblue4"))
  color <- segment_colors[[as.integer(good)+1]]
  
  segments(ci99_low, indices, ci99_high, indices, lwd=3, col=color[1])
  segments(ci95_low, indices, ci95_high, indices, lwd=3, col=color[2])
  points(sample_means, indices, pch=18, cex=0.6, col=color[3])
}

#a)
visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, distr_func=rnorm, mean=20, sd=3)

#3
#0

#b)
visualize_sample_ci(num_samples = 100, sample_size = 300, pop_size=10000, distr_func=rnorm, mean=20, sd=3)
#Yes, it's 99% ci and 95% ci are narrower than before.
#8

#c)
visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, distr_func=runif, min=17, max=35)
visualize_sample_ci(num_samples = 100, sample_size = 300, pop_size=10000, distr_func=runif, min=17, max=35)
#Compared to (a) and (b), they both hava wider width when sample_size is 100, and have narrower width when sample_size is 300.


#3
bookings <- read.table("./first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
plot(density(minday), main="Minute (of the day) of first ever booking", col="blue", lwd=2)
#a)
#i)
#length(minday)
mean(minday)
std_error <- sd(minday)/sqrt(length(minday))
std_error 
cat("95% confidence level is from", mean(minday)-1.96*std_error, "to", mean(minday)+1.96*std_error)

#ii)
compute_sample_mean <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  mean(resample)
}
compute_sample_mean(minday)
sample_means <- replicate(2000, compute_sample_mean(minday))

#iii)
plot(density(sample_means), lwd=0, col=rgb(0.0, 0.4, 0.0, 0.01), main="means of bootstrapped samples")
plot_sample_mean<-function(sample_i) {
  abline(v=sample_i, col=rgb(0.0, 0.4, 0.0, 0.01))
}
resamples_ <- data.matrix(sample_means)
apply(resamples_, 2, FUN = plot_sample_mean)

##
# plot(density(minday), lwd=0, ylim=c(0, 0.009), main="means of bootstrapped samples")
# plot_resample_mean<-function(sample_i) {
#     abline(v=resamples, col=rgb(0.0, 0.4, 0.0, 0.01))
# }
# resamples_ <- data.matrix(resamples)
# sample_means <- apply(resamples_, 2, FUN = plot_resample_mean)

#iv)
quantile(sample_means, probs = c(0.05, 0.95))

#b)
hour <- round(median(minday)/60, 0)
minute <- median(minday)%%60
cat(hour,":",minute)

#i)
median(minday)
#ii)
compute_sample_median <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  median(resample)
}
compute_sample_median(minday)
samples_medians <- replicate(2000, compute_sample_median(minday))

plot(density(minday), lwd=0, main="median of bootstrapped samples")
plot_resample_median<-function(sample_i) {
  #lines(density(sample_i), col="blue")
  abline(v=sample_i, col=rgb(0.5, 0.0, 0.3, 0.005))
}
resamples_m <- data.matrix(samples_medians)
apply(resamples_m , 2, FUN = plot_resample_median)

#iii)
quantile(samples_medians, probs = c(0.05, 0.95))

