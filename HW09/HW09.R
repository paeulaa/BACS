library(data.table)
ac_bundles_dt <- fread("piccollage_accounts_bundles.csv")
ac_bundles_matrix <- as.matrix(ac_bundles_dt[, -1, with=FALSE])
ac_bundles_dt
ac_bundles_matrix
#colnames(ac_bundles_matrix)
#a.
#i)6
#ii)
#Pellington Image
#Random, Monsterhigh, alien, KLL, newyearsparty

#b
#i
#1)
matrix <- data.frame(
  top5 <- apply(ac_bundles_matrix, 2, function(x){sort(x, decreasing = T)[1:5]})
)
matrix

#2)
library(lsa)
cos_m <- function(x){
  cos_m <- x
  for (i in c(1:165)){cos_m[i,i] <- -10000}
  return(cos_m)
}
automated_cossin <- function(x) {
  cossin <- cosine(x)
  nameLists <- colnames(cossin)
  recommand_matrix <- data.frame(
    cos_top5 <- apply(cos_m(cossin), 2, function(x){
      nameLists[order(x, decreasing = T)[1:5]]
    })
  )
  return (recommand_matrix)
}
automated_cossin(ac_bundles_matrix)

#3)
#top5: springrose, 8bit2, mmlm, julyfourth, tropicalparadise

#ii
#1)2)
library(lsa)
cos_m <- function(x){
  cos_m <- x
  for (i in c(1:165)){cos_m[i,i] <- -10000}
  return(cos_m)
}
automated_cossin <- function(x) {
  bundle_mean <- apply(ac_bundles_matrix, 2, mean)
  bundel_mean_matrix <- t(replicate(nrow(ac_bundles_matrix), bundle_mean))
  ac_bundles_mc_b <- ac_bundles_matrix - bundel_mean_matrix
  cor_sim <- cosine(ac_bundles_mc_b)
  nameLists <- colnames(cor_sim)
  recommand_matrix <- data.frame(
    cos_top5 <- apply(cos_m(cor_sim), 2, function(x){
      nameLists[order(x, decreasing = T)[1:5]]
    })
  )
  return (recommand_matrix)
}
automated_cossin(ac_bundles_matrix)


#3)
#top5: springrose, 8bit2, tropicalparadise, mmlm, julyfourth

#iii)
#1)2)
library(lsa)
cos_m <- function(x){
  cos_m <- x
  for (i in c(1:165)){cos_m[i,i] <- -10000}
  return(cos_m)
}
automated_cossin <- function(x) {
  bundle_mean <- apply(ac_bundles_matrix, 1, mean)
  bundle_mean_matrix <- t(replicate(ncol(ac_bundles_matrix), bundle_mean))
  tmp <- as.data.frame(t(bundle_mean_matrix), row.names=F)
  for (i in c(1:165)){
    colnames(tmp)[i] =  nameLists[i]
  }
  bundle_mean_matrix_rev <- tmp
  ac_bundles_mc_b <- ac_bundles_matrix - bundle_mean_matrix_rev
  cossin <- cosine(x)
  nameLists <- colnames(cossin)
  recommand_matrix <- data.frame(
    cos_top5 <- apply(cos_m(cossin), 1, function(x){
      nameLists[order(x, decreasing = T)[1:5]]
    })
  )
  return (recommand_matrix)
}
automated_cossin(ac_bundles_matrix)

#3)simple answer
#top5: springrose, 8bit2, mmlm, julyfourth, tropicalparadise

#c
#My intuition is totally different from the result of 
#geometric recommendations.
#Because my intuition is depend on the name of the bundles, 
#but the computational result is depend on the data similarity and the number of recommendation.
#Therefore, I think it will be totally different.

#d
#My computational result is different among cosine similarity, correlation, 
#and adjusted-cosine, because those quantities represent different physical entities. 
#The cosine similarity computes the similarity between two samples, 
#whereas the Pearson correlation coefficient computes the correlation between two 
#jointly distributed random variables.

#Q2
source('demo_simple_regression.R')
#a
qa <- interactive_regression() 
write.table(qa, file = "qa.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
qa <- read.table("./qa.txt")
lm(qa$y ~ qa$x)$coeff[[2]]
cor(qa)
#b
qb <- interactive_regression()
write.table(qb, file = "qb.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
qb <- read.table("./qb.txt")
lm(qb$y ~ qb$x)$coeff[[2]]
cor(qb)
#c
qc <- interactive_regression() 
write.table(qc, file = "qc.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
qc <- read.table("./qc.txt")
lm(qc$y ~ qc$x)$coeff[[2]]
cor(qc)
#d
qd <- interactive_regression() 
write.table(qd, file = "qd.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
qd <- read.table("./qd.txt")
lm(qd$y ~ qd$x)$coeff[[2]]
cor(qd)


#e.
Qe <- interactive_regression() 
write.table(Qe, file = "Qe.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
Qe <- read.table("./Qe.txt")
plot(Qe)

#f
Qf <- interactive_regression()
write.table(Qf, file = "Qf.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
Qf <- read.table("./Qf.txt")
plot(Qf)

#g
#source('demo_simple_regression.R')
pts <- interactive_regression() 
write.table(pts, file = "pts.txt", sep = " ", quote = FALSE, append = FALSE, na = "NA")
pts <- read.table("./pts.txt")
pts
summary(lm(pts$y ~ pts$x))
cor(pts)
pts_mean <- apply(pts, 2, mean)
pts_mean_matix <- t(replicate(nrow(pts), pts_mean))

pts_sd <- apply(pts, 2, sd)
pts_sd_matix <- t(replicate(nrow(pts), pts_sd))

pts_std <- (pts - pts_mean_matix)/pts_sd_matix
pts_std
summary(lm(pts_std$y ~ pts_std$x))
cor(pts_std)


