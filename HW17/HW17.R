insurance <- read.csv("insurance.csv")
insur <- na.omit(insurance)

#Q1
#a
ols_lm <- lm(charges ~ age + factor(sex) + bmi + children + factor(smoker) + factor(region), data = insur)
summary(ols_lm)
#b
library(rpart)
library(rpart.plot)
tree_lm <- rpart(charges ~ age + factor(sex) + bmi + children + factor(smoker) + factor(region), data = insur)
#(1)
rpart.plot(tree_lm)
#(2)
#6
#(3)
#4
#(4)
#26, 42, 41.5, 53.5
#(5)
#charges < 3621, charges < 21e+3, charges < 24e+3, charges < 12e+3

#Q2
fold_i_pe <- function(i, k, model, dataset, outcome) {
    folds <- cut(1:nrow(dataset), breaks=k, labels=FALSE)
    test_indices <- which(folds==i)
    test_set <- dataset[test_indices, ]
    train_set <- dataset[-test_indices, ]
    trained_model <- update(model, data = train_set)
    predictions <- predict(trained_model, test_set)
    dataset[test_indices, outcome]- predictions
}

#library(Metrics)
k_fold_mse <- function(model, dataset, outcome, k){
    shuffled_indicies <- sample(1:nrow(dataset))
    dataset <- dataset[shuffled_indicies,]
    fold_pred_errors <- sapply(1:k, function(kth) {
        fold_i_pe(kth, k, model, dataset, outcome)
      })
    pred_errors <- unlist(fold_pred_errors)
    
    mse <- function(errs){mean(errs^2)}
    c(is = mse(residuals(model)), oos = mse(pred_errors))
}

#(1)
ols_kfm <- k_fold_mse(ols_lm, insur, "charges", 10)
sqrt(ols_kfm[2])
#(2)
tree_kfm <- k_fold_mse(tree_lm, insur, "charges", 10)
sqrt(tree_kfm[2])

#Q3
set.seed(27935752)
train_indices <- sample(1:nrow(insur), size = 0.80*nrow(insur))
train_set <- insur[train_indices,]
test_set <- insur[-train_indices,]

bagged_learn <- function(model, dataset, b=100) {
    lapply(1:b, function(i) {
       #boot_index <- sample(1:nrow(dataset), norm(dataset), replace = TRUE)
       boot_index <- sample(nrow(dataset), replace = TRUE)
       boot_dataset <- dataset[boot_index,]
       boot_model <- update(model, data=boot_dataset)
       return(boot_model)
    })
}
bagged_predict <- function(bagged_models, new_data, b=100) {
    predictions <- lapply(1:b, function(i){
      predict(bagged_models[[i]], new_data)
    })
    apply(as.data.frame(predictions), 1, mean)
}

mse_oos <- function(actual, pred){
  sqrt(mean((actual-pred)^2))
}

#(1)
bagged_model <- bagged_learn(ols_lm, train_set, 100)
bagged_prediction <- bagged_predict(bagged_model, test_set, 100)
mse_oos(test_set$charges, bagged_prediction)
# actual_ols <- test_set$charges
# rmse_ols <- sqrt(mean((unlist(bagged_prediction) - actual_ols)^2))
# rmse_ols

#(2)
bagged_model <- bagged_learn(tree_lm, train_set, 100)
bagged_prediction <- bagged_predict(bagged_model, test_set, 100)
mse_oos(test_set$charges, bagged_prediction)
# actual_tree <- test_set$charges
# rmse_tree <- sqrt(mean((unlist(bagged_prediction) - actual_tree)^2))
# rmse_tree

#Q3-2
boost_learn <- function(model, dataset, n=100, rate=0.1) {
    predictors <- dataset[, 1:6]
      #dataset[,...] #get data frame of only predictor variables
    
    # Initialize residuals and models
    res <-dataset[, 7] # get vector of actuals to start
    models <- list()
    
    for(i in 1:n) {
      this_model <- update(model, data = cbind(charges=res, predictors))
      res <- res - rate*predict(this_model)#fitted(this_model) # update residuals with learning rate
      models[[i]] <-this_model
      # Store model
    }
    list(models=models, rate=rate)
}

#library(magrittr)
boost_predict <- function(boosted_learning, new_data) {
    boosted_models <- boosted_learning$models
    rate <- boosted_learning$rate
    n<-length(boosted_learning$models)
    #n <- nrow(new_data)
    
    predictions <- lapply(1:n, function(i){
      rate*predict(boosted_models[[i]], new_data)
    })
    # get predictions of new_data from each model
    pred_frame <- unname(as.data.frame(predictions)) #as.data.frame(predictions) |> unname()
    apply(pred_frame, 1, sum)
    # apply a sum over the columns of predictions, weighted by learning rate
}

#(2)
boosted_model <- boost_learn(ols_lm, train_set, 100, 0.1)
boost_prediction <- boost_predict(boosted_model, test_set)
mse_oos(test_set$charges, unlist(boost_prediction))

# actual_ols <- test_set$charges
# rmse_ols <- sqrt(mean((unlist(boost_prediction) - actual_ols)^2))
# rmse_ols

#(3)
boosted_model <- boost_learn(tree_lm, train_set, 100, 0.1)
boost_prediction <- boost_predict(boosted_model, test_set)
mse_oos(test_set$charges, unlist(boost_prediction))
# actual_tree <- test_set$charges
# rmse_tree <- sqrt(mean((unlist(boost_prediction) - actual_ols)^2))
# rmse_tree

#Q4
#(1)
x<-NULL
pre <- 1000000
for(i in 1:100) {
  bagged_model <- bagged_learn(tree_lm, train_set, i)
  bagged_prediction <- bagged_predict(bagged_model, test_set, i)

  actual_tree <- test_set$charges
  rmse_tree <- sqrt(mean((unlist(bagged_prediction) - actual_tree)^2))
  #if(rmse_tree > pre) break

  pre <- rmse_tree
  x <- rbind(x, rmse_tree)
  #cat(rmse_ols, sep = "\n")
}
x
# 
# y<-NULL
# pre <- 1000000
# for(i in 1:100) {
#   boosted_model <- boost_learn(tree_lm, train_set, i, 0.1)
#   boost_prediction <- boost_predict(boosted_model, test_set)
#   
#   actual_tree <- test_set$charges
#   rmse_tree <- sqrt(mean((unlist(boost_prediction) - actual_ols)^2))
#   if(rmse_tree > pre) break
#   
#   pre <- rmse_tree 
#   y <- rbind(y, rmse_tree)
#   #cat(rmse_ols, sep = "\n")
# }
# y
# 
pre <- 1000000
for(i in 1:30){
  tree_stump <- rpart(charges ~ age + factor(sex) + bmi +
                            children + factor(smoker) + factor(region),
                          data = insur, cp=0, maxdepth=i)

  boosted_model <- boost_learn(tree_stump, train_set, 30, 0.1)
  boost_prediction <- boost_predict(boosted_model, test_set)
  rmse_tree <- mse_oos(test_set$charges, unlist(boost_prediction))
  cat(rmse_tree , sep = "\n")
  if(rmse_tree > pre) break

  pre <- rmse_tree
 
}

pre <- 1000000
for(i in 1:30){
  tree_stump <- rpart(charges ~ age + factor(sex) + bmi +
                        children + factor(smoker) + factor(region),
                      data = insur, cp=0, maxdepth=i)
  
  bagged_model <- bagged_learn(tree_lm, train_set, 30)
  bagged_prediction <- bagged_predict(bagged_model, test_set, 30)
  rmse_tree <- mse_oos(test_set$charges, bagged_prediction)
  cat(rmse_tree , sep = "\n")
  if(rmse_tree > pre) break
  
  pre <- rmse_tree
}

boosted_lm_algo <- function(tree){
  boosted_list <- boost_learn(tree, train_set, 30) 
  boosted_predict_list <- boost_predict(boosted_list, test_set) 
  mse_oos(test_set$charges, unlist(boosted_predict_list))
}

num <- 1
pre_oos <- 100000000
oos <- 100000000
while (pre_oos >= oos) {
  pre_oos <<- oos
  old_tree_stump <- rpart(charges ~ age + sex + bmi + children + 
                            smoker + region, data = insur, cp=0, maxdepth=i)
      oos <<- boosted_lm_algo(old_tree_stump)
      cat(num, ":", oos, "\n")
      num <<- num + 1
}



