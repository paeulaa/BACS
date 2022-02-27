my_data <- read.csv("./customers.csv")
my_data

#1.What is the 5th element in the original list of ages?
my_data[5,]

#2.What is the fifth lowest age?
fifth_lowest<-sort(my_data$age, FALSE)[5]
fifth_lowest

#3.Extract the five lowest ages together.
five<-sort(my_data$age, FALSE)[1:5]
five

#4.Get the five highest ages by first sorting them in decreasing order first.
sort_data <- sort(my_data$age, decreasing = TRUE)
sort_data[1:5]

#5.What is the average (mean) age?
mean(sort_data)

#6.What is the standard deviation of ages?
sd(sort_data)

#7."age_diff" = the difference between each age and the mean age.
age_diff <- my_data$age-mean(my_data$age)
age_diff

#8.What is the average “difference between each age and the mean age”
mean(age_diff)

#9.Visualize the raw data: (a) histogram, (b) density plot, (c) boxplot+stripchart
#(a) histogram
hist(my_data$age, main="histogram", xlab="numbers", ylab="ages") 
#(b) density plot
plot(density(my_data$age), main = "Density Plot", xlab = "ages", ylab = "density")
#(c) boxplot+stripchart
boxplot(my_data$age, main = "boxplot + stripchart", xlab = "ages", horizontal = TRUE)
stripchart(my_data$age, add = TRUE, method = "stack")

