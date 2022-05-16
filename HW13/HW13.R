auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model_year", "origin", "car_name")
cars <- na.omit(auto)

cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), 
            log(horsepower), log(weight), log(acceleration), model_year,
            factor(origin)), na.action=na.exclude) 
head(cars_log)

#Q1
#a
#i
var4_log <- with(cars, data.frame(log(cylinders), 
                                  log(displacement), log(horsepower), log(weight))) 
#ii
round(cor(var4_log), 2)
eigen <- eigen(cor(var4_log))
eigen$values
#pca <- prcomp(sub_log, scale. = TRUE)
#(pca$sdev)^2
#eigen$values[1] /sum(eigen$values)

#iii
# Move the center of the coordinate axis to the center of the data, 
# and then rotate the coordinate axis, so that the variance of the data 
# on the C1 axis is the largest, that is, the projections of all n data 
# individuals in this direction are the most dispersed. Means more 
# information is preserved. C1 becomes the first principal component .
# https://zh.wikipedia.org/zh-tw/%E4%B8%BB%E6%88%90%E5%88%86%E5%88%86%E6%9E%90 

#b
#i
cars_log <- na.omit(cars_log)
pca <- prcomp(var4_log, scale. = TRUE)
cars_log$PC1 <- pca$x[,1]
head(cars_log)
#ii
summary(lm(log.mpg. ~ PC1 + log.acceleration. + model_year + factor.origin., data = cars_log))
#iii
library(dplyr)
cars_log_std <- cars_log %>% mutate_if(is.numeric,scale)
summary(lm(log.mpg. ~ PC1 + log.acceleration. + model_year + factor.origin., data = cars_log_std))

summary(lm(cars_log$log.mpg. ~ PC1 + log.acceleration. + model_year + factor.origin., data = cars_log_std))
acceleration_mc <- scale(
  cars_log$log.acceleration., center=TRUE, scale=FALSE)
model_year_mc <- scale(
  cars_log$model_year, center=TRUE, scale=FALSE)
origin_mc <- scale(as.numeric(
  factor(cars_log$origin)), center=TRUE, scale=FALSE)
mpg_mc <- scale(
  cars_log$log.mpg., center=TRUE, scale=FALSE)

dec4_mc <- prcomp(scale(var4_log))
cars_log$dec4_pca_x_mc <- dec4_mc$x[, "PC1"]

summary(lm(mpg_mc ~ dec4_pca_x_mc + acceleration_mc + 
             model_year_mc + factor.origin., data=cars_log))





#Q2
#a
library("readxl")
sa <- read_excel("security_questions.xlsx")

#round(cor(sa), 2)
sa_eigen <- eigen(cor(sa))
sa_eigen$values[1] /sum(sa_eigen$values)
#pca <- prcomp(sub_log, scale. = TRUE)
sa_eigen$values/sum(sa_eigen$values)
#b
which(sa_eigen$values > 1)
sa_pca <- prcomp(sa, scale. = TRUE)
screeplot(sa_pca, type="lines")
#one.

#Q3
#a
library(devtools)
install_github("soumyaray/compstatslib") 
library(compstatslib)

s1 <- interactive_pca()
# Qs1 <- capture.output(s1)
# write.table(s1$points, file = "s1.txt")
# s1 <- read.table("./s1.txt")
s2 <- interactive_pca()












