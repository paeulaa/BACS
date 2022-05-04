auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", "origin", "car_name")
cars <- auto
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), 
                                  log(horsepower), log(weight), log(acceleration), 
                                  model_year, origin)) 
#Q1)
#a.
regr <- lm(log.mpg. ~ log.cylinders.+ log.displacement.+ 
             log.horsepower.+ log.weight.+ 
             log.acceleration.+ model_year+ 
             factor(origin), data = cars_log)
summary(regr)
#none of them.
#https://www.statology.org/significance-codes-in-r/

#b
regr_o <- lm(mpg ~ cylinders+ displacement+ 
             horsepower+ weight+ acceleration+ model_year+ 
              factor(origin), data = cars)
summary(regr_o)
#horsepower, origin, accerlation have significant effect on mpg.
#Because people can see the hidden trend differences among datas with logistic regression.

#c
#log.cylinders. has insignificant effect on mpg.
#log.horsepower., log.weight., and log.acceleration. has opposite effect on log.mpg., because their correlation is negative.
#Why might this be???

#a.
#1
regr_wt <- lm(mpg ~ weight, data = cars, na.action=na.exclude)
#2
regr_wt_log <- lm(log.mpg. ~ log.weight., data = cars_log, na.action=na.exclude)
#3
plot(density(regr_wt$residuals), lwd = 2, col = "red")
plot(cars$weight, regr_wt$residuals, main="Scatterplot", col = "red")
plot(density(regr_wt_log$residuals), lwd = 2, col = "blue", main = "density plot")
plot(cars_log$log.weight., regr_wt_log$residuals, main="Scatterplot", col = "blue")
#4
library(ggplot2)
ggplot(cars, aes(x = weight, y = regr_wt$residuals)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplot(cars_log, aes(x = log.weight., y = regr_wt_log$residuals)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
#logistic regression has a better residuals distribution, 
#because it is linear, constant vaiance, and independent.
# On the other hand, regression of raw data doesn't have constant variance properties.
# https://twgreatdaily.com/1G0hNHMBd4Bm1__YeN_H.html

#5
summary(regr_wt_log)
#The number of slope in regression is -1.0583. 
#It means that if log.weight. increase a unit, then log.mpg will increase 11.529+(-1.0583).

#c
#1
#cars_log_ <- cars_log[complete.cases(cars_log),]
boot_regr <- function(model, dataset) {
  boot_index <- sample(1: nrow(dataset), replace = TRUE)
  data_boot <- dataset[boot_index,]
  regr_boot <- lm(model, data = data_boot)
  regr_boot$coefficients
}
coeffs <- replicate(300, boot_regr(log.mpg.~log.weight., cars_log))
quantile(coeffs["log.weight.",], c(0.025, 0.975))

#2
wt_regr_log <- lm(log.mpg.~log.weight., cars_log)
confint(wt_regr_log)

#Q2
#1
regr_log <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. +
                 log.weight. + log.acceleration. + model_year +
                 factor(origin), data=cars_log)
#a
weight_regr <- lm(log.weight. ~ log.displacement. + log.horsepower. +
                    log.acceleration. + model_year + factor(origin), 
                  data=cars_log, na.action = na.exclude)
r2_weight <- summary(weight_regr)$r.squared
vif_weight <- 1/(1- r2_weight)
vif_weight

#b
library(car)
#1
vif(regr_log)

#2
regr_log1 <- lm(log.mpg. ~ log.cylinders. + log.horsepower. +
                 log.weight. + log.acceleration. + model_year +
                 factor(origin), data=cars_log)
vif(regr_log1)

#3
regr_log2 <- lm(log.mpg. ~ log.cylinders. + log.weight. + 
                  log.acceleration. + model_year +
                  factor(origin), data=cars_log)
vif(regr_log2)

regr_log3 <- lm(log.mpg. ~ log.weight. + 
                  log.acceleration. + model_year +
                  factor(origin), data=cars_log)
vif(regr_log3)

#4
regr_log3
summary(regr_log3)

#c
summary(regr_log)
# Yes, we lost horsepower.
# We sloghtly hurt the salience of the regression model, 
# because the previous model's R squaerd is 0.8919, and the new model's R squaerd is 0.8856.

#d
#1
#For only formula, if an independent variable has no correlation 
#with other independent variables, the value of R-squared will close to 0.
#Therefore, the VIF will be 1/(1-0) = 1.(closing to 1)

#2
# 5 < 1/(1-R^2), R = +- 0.8944272
# 10 < 1/(1-R^2), R = +- 0.6324555

#Q3
origin_colors = c("blue", "darkgreen", "red")
with(cars_log, plot(log.weight., log.mpg., pch=origin, col=origin_colors[origin]))

cars_us <- subset(cars_log, origin==1)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us)
abline(wt_regr_us, col=origin_colors[1], lwd=2)

cars_us <- subset(cars_log, origin==2)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us)
abline(wt_regr_us, col=origin_colors[2], lwd=2)

cars_us <- subset(cars_log, origin==3)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us)
abline(wt_regr_us, col=origin_colors[3], lwd=2)

#no, they are the same.





