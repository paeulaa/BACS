auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", "origin", "car_name")
cars <- auto
cars <- cars[,-9]
cars <- cars[,-4]
cars <- cars[,-3]

cars_log <- with(cars, data.frame(log(mpg), log(weight), log(acceleration), 
                                  model_year, factor(origin))) 
cars_log

#Q1
#a
#i
log_mean <- mean(cars_log$log.weight.)
light_cars <- subset(cars_log, log.weight. < log_mean)
heavy_cars <- subset(cars_log, log.weight. >= log_mean)

subset1 <- cars[ which(cars$weight < mean(cars$weight)),]
subset2 <- cars[ which(cars$weight>=mean(cars$weight)),]

subset1_log <- with(subset1, 
                    data.frame(log(mpg), 
                               log(weight), 
                               log(acceleration), 
                               model_year, origin))
subset2_log <- with(subset2, 
                    data.frame(log(mpg), 
                               log(weight), 
                               log(acceleration), 
                               model_year, origin))
head(light_cars)
head(heavy_cars)
head(subset1_log)
head(subset2_log)
#ii
library(ggplot2)
p <- ggplot() + 
  #light_cars
  geom_point(data = light_cars, aes(x = log.mpg., y = log.acceleration.), 
                 color='darkblue') + 
  #heavy_cars
  geom_point(data = heavy_cars, aes(x = log.mpg., y = log.acceleration.), 
             color='darkred') 
p 
#iii
p + geom_smooth(data = light_cars, aes(x = log.mpg., y = log.acceleration.), 
               fill="blue", colour="darkblue", method = "lm") + 
    geom_smooth(data = heavy_cars, aes(x = log.mpg., y = log.acceleration.), 
               fill="red", colour="darkred", method = "lm")
#b
summary(lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor.origin., data = light_cars))
summary(lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor.origin., data = heavy_cars))

#c
#light v.s. heavy

#Q2
#a
#answers

#b
#i
summary(lm(log.mpg. ~ log.weight. + log.acceleration. + 
             model_year + factor.origin., data = cars_log))
#ii
summary(lm(log.mpg. ~ log.weight. + log.acceleration. + 
             model_year + factor.origin. + log.weight.*log.acceleration., data = cars_log))
#iii
weight_mc <- scale(cars_log$log.weight., center=TRUE, scale=FALSE)
accerlation_mc <- scale(cars_log$log.acceleration., center=TRUE, scale=FALSE)

summary(lm(log.mpg. ~ weight_mc + accerlation_mc + 
             model_year + factor.origin. + weight_mc*accerlation_mc, data = cars_log))
#iv.
w_X_a <- cars_log$log.weight.*cars_log$log.acceleration.
intereaction_regr <- lm(w_X_a ~ log.weight. + log.acceleration. + model_year + 
                          factor.origin., data = cars_log)
interaction_ortho <- intereaction_regr$residuals
summary(lm(log.mpg. ~ log.weight. + log.acceleration. + interaction_ortho + model_year + 
             factor.origin., data=cars_log))

#c
#correlation of ii, iii, iv
cor(cars_log$log.weight., cars_log$log.weight.*cars_log$log.acceleration.)
cor(cars_log$log.acceleration., cars_log$log.weight.*cars_log$log.acceleration.)
cor(weight_mc, weight_mc*accerlation_mc)
cor(accerlation_mc, weight_mc*accerlation_mc)
cor(cars_log$log.weight., interaction_ortho)
cor(cars_log$log.acceleration., interaction_ortho)

# car_log <- as.numeric(unlist(cars_log))
# round(cor(cbind(car_log, interaction_ortho)), 2)

#Q3
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", "origin", "car_name")
cars <- auto
cars <- cars[,-9]
cars <- cars[,-4]

cars_log_ <- with(cars, data.frame(log(mpg), log(cylinders), log(weight), log(acceleration), 
                                  model_year, factor(origin))) 
cars_log_

#a
#i
summary(lm(log.weight. ~ log.cylinders., data = cars_log_))
#Yes, it has 0.1% significant effect on weight.

#ii
summary(lm(log.mpg. ~ log.weight. + log.acceleration. 
           + model_year + factor.origin., data = cars_log_))
#Yes, it has 0.1% significant effect on weight.

#b
wc_regr <- summary(lm(log.weight. ~ log.cylinders., data = cars_log_))

mw_regr <- summary(lm(log.mpg. ~ log.weight. + log.acceleration. 
                  + model_year + factor.origin., data = cars_log_))
wc_regr$coefficients[2]*mw_regr$coefficients[2]

#c
boot_mediation <- function(model1, model2, dataset) {
  boot_index <- sample(1:nrow(dataset), replace=TRUE)
  data_boot <- dataset[boot_index, ]
  regr1 <- lm(model1, data_boot)
  regr2 <- lm(model2, data_boot)
  return(regr1$coefficients[2] *regr2$coefficients[2])
}

set.seed(42)

indirect <- replicate(2000, boot_mediation(wc_regr, mw_regr, cars_log_))
quantile(indirect, probs=c(0.025, 0.975))

plot(density(indirect), col = "blue")
abline(v=quantile(indirect, probs=c(0.025, 0.975)), col = "pink", lwd = 3)




