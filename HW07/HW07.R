#Q1
#1
data1 <- read.csv("./pls-media/pls-media1.csv")
data2 <- read.csv("./pls-media/pls-media2.csv")
data3 <- read.csv("./pls-media/pls-media3.csv")
data4 <- read.csv("./pls-media/pls-media4.csv")

max.len <- max(length(data1$INTEND.0), length(data2$INTEND.0), length(data3$INTEND.0), length(data4$INTEND.0))
datas <- data.frame(
  d1 = c(data1$INTEND.0, rep(NA, max.len - length(data1$INTEND.0))),
  d2 = c(data2$INTEND.0, rep(NA, max.len - length(data2$INTEND.0))),
  d3 = c(data3$INTEND.0, rep(NA, max.len - length(data3$INTEND.0))),
  d4 = c(data4$INTEND.0, rep(NA, max.len - length(data4$INTEND.0)))
)
datas

means <- sapply(datas, mean, na.rm=TRUE)
means

#2
boxplot(data1$INTEND.0, data2$INTEND.0, data3$INTEND.0, data4$INTEND.0, horizontal = TRUE, col = "skyblue", main = "distribution and mean of intention")
mean1 <- mean(data1$INTEND.0)
mean2 <- mean(data2$INTEND.0)
mean3 <- mean(data3$INTEND.0)
mean4 <- mean(data4$INTEND.0)
abline(v = mean1, col = "red", lwd = 2)
abline(v = mean2, col = "green", lwd = 2)
abline(v = mean3, col = "yellow", lwd = 2)
abline(v = mean4, col = "pink", lwd = 2)
#3
# People prefer to share text with each other, and they don't like to share picture and audio. 

#Q2
#1
#μ1 = μ2 = μ3 = μ4
#μ1 ≠ μ2; μ1 ≠ μ3; μ1 ≠ μ4; μ2 ≠ μ3; μ2 ≠ μ4; μ3 ≠ μ4;
#2
means <- c(mean1, mean2, mean3, mean4)
mean(means)
sstr <- (length(data1$INTEND.0)*((mean1 - mean(means))^2)+length(data2$INTEND.0)*((mean2 - mean(means))^2)+
  length(data3$INTEND.0)*((mean3 - mean(means))^2)+length(data4$INTEND.0)*((mean4 - mean(means))^2))
df_mstr <- 4 - 1
mstr <- sstr/df_mstr
mstr

var1 <- sd(data1$INTEND.0)^2
var2 <- sd(data2$INTEND.0)^2
var3 <- sd(data3$INTEND.0)^2
var4 <- sd(data4$INTEND.0)^2
vars <- c(unname(sapply(datas, var, na.rm=TRUE)))
sse<- (length(data1$INTEND.0)-1)*vars[1]+(length(data2$INTEND.0)-1)*vars[2]+
  (length(data3$INTEND.0)-1)*vars[3]+(length(data4$INTEND.0)-1)*vars[4]
df_mse <- length(data1$INTEND.0) + length(data2$INTEND.0) + length(data3$INTEND.0) + 
  length(data4$INTEND.0)- 4
mse <- sse/df_mse
mse
f_value <- mstr/mse
f_value
#qf(p=0.95, df1=df_mstr, df2=df_mse)
p_value <- pf(f_value, df_mstr, df_mse, lower.tail=FALSE)
p_value
#f-value > p-value in lower tail, so we should reject our null hypothesis.

#3
library(reshape2)
test <- melt(datas, na.rm = TRUE, id.vars = NULL, variable.name = "media", value.name = "intend")
test
oneway.test(test$intend~factor(test$media), var.equal=TRUE)
summary(aov(test$intend~factor(test$media)))
#4
anova_model <- aov(test$intend~factor(test$media))
TukeyHSD(anova_model, conf.level= 0.01)

#5

#Q3
#a
#μ1 = μ2 = μ3 = μ4
#μ1 ≠ μ2; μ1 ≠ μ3; μ1 ≠ μ4; μ2 ≠ μ3; μ2 ≠ μ4; μ3 ≠ μ4;
#b
sales_ranks <- rank(test$intend, na.last = NA)
sales_ranks

group_ranks <- split(sales_ranks, test$media)
group_ranks

group_ranksums <- sapply(group_ranks, sum) #, na.rm=TRUE)))
group_ranksums

group_length <- sapply(group_ranks, length)
group_length

N <- length(test$intend)
N
H <- (12/N*(N+1))*sum((group_ranksums^2)/group_length)-3*(N+1)
H
kw_p <- 1 - pchisq(H, df=4-1)
kw_p

#c
kruskal.test(intend ~ media, data = test)
#kw_p = 0, p-value of kruskal-Wallis rank sum test is 0.03166. They are similar.

#d
library(FSA)
dunnTest(intend ~ media, data = test, method = "bonferroni")















