library("readxl")
sa <- read_excel("security_questions.xlsx")

#Q1
#a
sa_eigen <- eigen(cor(sa))
sa_eigen$values/sum(sa_eigen$values)
sa_pca <- prcomp(sa, scale. = TRUE)

library(magrittr)
noise <- data.frame(replicate(ncol(sa), rnorm(nrow(sa))))
noise_pca <- prcomp(noise, scale. = TRUE)
var_sa <- (sa_pca$sdev)^2

screeplot(noise_pca, type="lines", ylim = c(0,10))
lines(var_sa, type="b", col = "gray")
abline(h=1, col = "darkgray")

#b Horn’s Parallel Analysis
sa_pca <- prcomp(sa, scale. = TRUE)
sim_noise_ev <- function(n, p) {
  noise<-data.frame(replicate(p, rnorm(n)))
  eigen(cor(noise))$values
}
evalues_noise<- replicate(100, sim_noise_ev(ncol(sa), nrow(sa)))
evalues_mean <- apply(evalues_noise, 1, mean)
screeplot(sa_pca, type="lines", ylim = c(0,45))
lines(evalues_mean, type="b", col = "gray")
abline(h=1,lty ="dotted")
#Only one major dimensions seems to exist in these correlates.

#Q2
library(psych)
sa_principal <- principal(sa, nfactor=3, rotate="none", scores=TRUE)
#a ?????
sa_principal$loadings
#b
sa_eigen$values[1:3] /sum(sa_eigen$values)
#c
sa_principal$uniquenesses
#In Q2, its u2 is higer than 50%, so it cannot fully interpreted by these three PCAS.
#d
#PC2 & PC3
#e
#The higer component represent the higer agreement, and the other lower components represent the lower agreement.

#Q3
sa_pca_rot <- principal(sa, nfactor=3, rotate="varimax", scores=TRUE)
#a
sa_principal$loadings
sa_pca_rot$loadings
#Yes, they are different.

#b
#The first value is smaller than PCA, and the second and the third values are bigger than PCA.

#c
#RC2 RC3 are still similar.

#d
#Yes, after reotating our PCA, we can interpret those datas easier.

#e
sa_pca_rot <- principal(sa, nfactor=2, rotate="varimax", scores=TRUE)
#Yes, because PC2 and PC3 are similar, and we only need one to represent the propertties of original data.












