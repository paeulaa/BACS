# Load the data and remove missing values
cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", 
                 "model_year", "origin", "car_name")
cars$car_name <- NULL
cars <- na.omit(cars)

# Shuffle the rows of cars
set.seed(27935752)
cars <- cars[sample(1:nrow(cars)),]

# Create a log transformed dataset also
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), log(horsepower), log(weight), log(acceleration), model_year, origin))
# Linear model of mpg over all the variables that don’t have multicollinearity
cars_lm <- lm(mpg ~ weight + acceleration + model_year + factor(origin), data=cars)
# Linear model of log mpg over all the log variables that don’t have multicollinearity
cars_log_lm <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin),  
                  data=cars_log)
# Linear model of log mpg over all the log variables, including multicollinear terms!
cars_log_full_lm <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. + 
                         log.weight. + log.acceleration. + model_year + factor(origin),
                       data=cars_log)
#Q1
#(a)
set.seed(27935752)
train_indices <- sample(1:nrow(cars_log), size = 0.70*nrow(cars_log))
train_set <- cars_log[train_indices,]
#lm_trained <- lm(mpg ~ displacement, data = train_set)
lm_trained <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin), data = train_set)
lm_trained

#(b)
test_set <- cars_log[-train_indices,]
mpg_predicted <- predict(lm_trained, test_set)
head(mpg_predicted)
#1
# mpg_fitted <- fitted(lm_trained)
# mpg_fitted <- lm_trained$fitted.values
# fit_error <- train_set$log.mpg. - mpg_fitted
# fit_error <- residuals(lm_trained)
mse_is <- mean((train_set$log.mpg - fitted(lm_trained))^2)
mse_is <- mean(residuals(lm_trained)^2)
mse_is
#2
mpg_actual <- test_set$log.mpg.
mse_oos <- mean((mpg_predicted - mpg_actual)^2)
mse_oos

#(c)
head(mpg_actual)
head(mpg_predicted)
pred_err <- mpg_actual - mpg_predicted
head(pred_err) 

#Q2
#(a)
cars_lm_mse_is <- mean((cars$mpg - fitted(cars_lm))^2)
cars_lm_mse_is  <- mean(residuals(cars_lm)^2)
cars_lm_mse_is 

cars_log_lm_mse_is <- mean((cars_log$log.mpg. - fitted(cars_log_lm))^2)
cars_log_lm_mse_is  <- mean(residuals(cars_log_lm)^2)
cars_log_lm_mse_is 

cars_log_full_lm_mse_is <- mean((cars_log$log.mpg. - fitted(cars_log_full_lm))^2)
cars_log_full_lm_mse_is  <- mean(residuals(cars_log_full_lm)^2)
cars_log_full_lm_mse_is 

#cars_log_full_lm has the best, and cars_lm_mse has the worst

#(b)
library(magrittr)
k_fold_mse <- function(data, k, model){
  data_shuffle_indices <- sample(1:nrow(data),replace = F)
  data_shuffle <- data[data_shuffle_indices,]
  fold_indices <- cut(1:nrow(data), k, labels = FALSE)
  f <- format(terms(model)) %>% paste(., collapse = " ") %>% as.formula()#to reuse the formula in the model#to reuse the formula in the model
  fold_pred_errors <- fold_indices
  for(i in 1:k){
    test_set <- data_shuffle[fold_indices == i,]
    train_set <- data_shuffle[fold_indices != i,]
    k_model <- lm(f,train_set)
    predicted <- predict(k_model, test_set)
    formula <- k_model$model %>% names()
    real_value <- test_set[,colnames(test_set) == formula[1]]
    fold_pred_errors[fold_pred_errors==i] <- (real_value - predicted)
  }
  return(mean(fold_pred_errors^2))
}
k_fold_mse(cars, 10, cars_lm)
k_fold_mse(cars_log, 10, cars_log_lm)
k_fold_mse(cars_log, 10, cars_log_full_lm)

#(c)
k_fold_mse(cars_log, 392, cars_log_lm)














