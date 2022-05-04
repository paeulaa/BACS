#Q1
source("demo_simple_regression_rsq.r")
s1 <- interactive_regression_rsq()
s2 <- interactive_regression_rsq()
s3 <- interactive_regression_rsq()
s4 <- interactive_regression_rsq()

write.table(s1, file = "s1.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
s1 <- read.table("./s1.txt")
plot_regr_rsq(s1)
write.table(s2, file = "s2.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
s2 <- read.table("./s2.txt")
plot_regr_rsq(s2)
write.table(s3, file = "s3.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
s3 <- read.table("./s3.txt")
plot_regr_rsq(s3)
write.table(s4, file = "s4.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
s4 <- read.table("./s4.txt")
plot_regr_rsq(s4)

#a
#s1-R squared: 0.98
#s2-R squared: 0.33
#s1 has a stronger R squared
#b
#s3-R squared: 0.4
#s4-R squared: 0.02
#s3 has a stronger R squared
#c
#s2 has a larger SSR, larger SSE, larger SST
#d
#s4 has a larger SSR, larger SSE, larger SST

#Q2
#a
salaries <- read.csv("programmer_salaries.txt", sep="\t")
salaries_reg <- lm(Salary ~ Experience + Score + Degree, data = salaries)
summary(salaries_reg)
salaries_reg$fitted.values[1:5]
salaries_reg$residuals[1:5]
#b
#i
X_1 <- t(matrix(1, ncol = 20))
X <- cbind(X_1, salaries$Experience, salaries$Score, salaries$Degree)
X
#ii
y <- salaries$Salary
y
#iii
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat
#iv
y_hat <- X %*% beta_hat
y_hat
res <- y - y_hat
res
#v
SSE <- sum((y - y_hat)^2)
SSE
RSquared <- cor(y, y_hat)^2
SST <- SSE/(1-RSquared)
SST
SSR <- SST - SSE
SSR
#c
RSquared_i <- SSR/SST
RSquared_i
RSquared_ii <- cor(y, y_hat)^2
RSquared_ii

#Q3
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
#a
#i
plot(auto$mpg, auto$cylinders, pch = 19, col = "lightblue", main = "mpg-cylinders")
abline(lm(auto$cylinders ~ auto$mpg), col = "green", lwd = 3)
plot(auto$mpg, auto$displacement, pch = 19, col = "lightblue", main = "mpg-displacement")
abline(lm(auto$displacement ~ auto$mpg), col = "green", lwd = 3)
plot(auto$mpg, auto$horsepower, pch = 19, col = "lightblue", main = "mpg-horsepower")
abline(lm(auto$horsepower ~ auto$mpg), col = "green", lwd = 3)
plot(auto$mpg, auto$weight, pch = 19, col = "lightblue", main = "mpg-weight")
abline(lm(auto$weight ~ auto$mpg), col = "green", lwd = 3)
plot(auto$mpg, auto$model_year, pch = 19, col = "lightblue", main = "mpg-model_year")
abline(lm(auto$model_year ~ auto$mpg), col = "green", lwd = 3)
plot(auto$mpg, auto$origin, pch = 19, col = "lightblue", main = "mpg-origin")
abline(lm(auto$origin ~ auto$mpg), col = "green", lwd = 3)
#ii
#auto_m <- auto[, -9]
auto_m <- data.matrix(auto)
#as.numeric(auto_m)
#auto_m[is.na(auto_m)] <- 0
res <- cor(auto_m, use="pairwise.complete.obs")
round(res, 2)
#corrplot(res2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#iii
library(corrplot)
corrplot(round(res, 2), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#displacement, weight, cylinders, horsepower
#iv
# Name
#v
# (horsepower, weight) (horsepower, cylinders) (horsepower, displacement)
# (weight, cylinders) (weight, displacement) (cylinders, displacement)

#b
#i
auto_m2 <- as.data.frame(auto_m)
mpg_rgm <- lm(mpg ~ cylinders + displacement + horsepower + 
                weight + acceleration + model_year + 
                factor(origin), data = auto_m2)
summary(mpg_rgm)
#displacement
#ii
mpg_rgm$coefficients
#The abs number of cylinders's coefficient is the biggest, 
#so it will efficiently increase mpg the most.

#c
#i
#auto_m[is.na(auto_m)] <- 0
auto_ <- auto_m[complete.cases(auto_m),]
auto_ <- auto_[, -8]
col_mean <- apply(auto_, 2, mean)
mean_matrix <- t(replicate(nrow(auto_), col_mean))
col_sd <- apply(auto_, 2, sd)
sd_matrix <- t(replicate(nrow(auto_), col_sd))
auto_std <- (auto_ - mean_matrix)/sd_matrix

mpg_reg_all <- lm(mpg ~ cylinders + displacement + horsepower + 
                    weight + acceleration + model_year + car_name, as.data.frame(auto_std))
summary(mpg_reg_all)
#Yes, in standardize type, it is easier to compare.

#ii
summary(lm(mpg ~ factor(origin) , as.data.frame(auto_m)))
summary(lm(mpg ~ car_name , as.data.frame(auto_m)))
summary(lm(mpg ~ acceleration , as.data.frame(auto_m)))
summary(lm(mpg ~ model_year , as.data.frame(auto_m)))
#origin is the most significant.

#iii
plot(density(mpg_reg_all$residuals), col = "skyblue", lwd = 3)
abline(v=mean(mpg_reg_all$residuals), col = "green", lwd = 3)
#Yes, they are normally distributed and centered around zero








