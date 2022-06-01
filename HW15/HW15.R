#https://github.com/sem-in-r/seminr/issues/141
library(seminr)
sec = read.csv("security_data_sem.csv")
#a
#Measurement Model
sec_mm <- constructs(
  composite("TRUST", multi_items("TRST", 1:4)),
  composite("SEC", multi_items("PSEC", 1:4)),
  composite("REP", multi_items("PREP", 1:4)),
  composite("INV", multi_items("PINV", 1:3)),
  composite("POL", multi_items("PPSS", 1:3)),
  composite("FAML", multi_items("FAML", 1:1)),
  interaction_term(iv ="REP", moderator ="POL", method = orthogonal)
)
sec_mm

#Structural Model
sec_sm <- relationships(
  paths(from = c("REP", "INV", "POL", "FAML", "REP*POL"), to = "SEC"),
  paths(from = "SEC", to = "TRUST")
)
sec_sm
#b
#1
sec_pls <- estimate_pls(data = sec, 
                        measurement_model = sec_mm, 
                        structural_model = sec_sm)
plot(sec_pls)
#2
sec_report <- summary(sec_pls)
sec_report$weights
sec_report$loadings
#3
sec_report$path
#4
boot_pls <- bootstrap_model(sec_pls, nboot= 1000, cores = 4, seed = NULL)
boot_pls_report <- summary(boot_pls)
CI_2.5 <- boot_pls_report$bootstrapped_paths[, "2.5% CI"]
CI_97.5 <-  boot_pls_report$bootstrapped_paths[, "97.5% CI"]
tvalues <- boot_pls_report$bootstrapped_paths[, "T Stat."]
data.frame(tvalues, CI_2.5, CI_97.5)

#b
#a
sec_cf_mm <- as.reflective(sec_mm)
sec_cf_sm <- sec_sm
#b
#1
sec_cf_pls <- estimate_cbsem(data = sec, 
                        measurement_model = sec_cf_mm, 
                        structural_model = sec_cf_sm)
library(semPlot)
plot(sec_cf_pls)
#2
sec_cf_report <- summary(sec_cf_pls)
#sec_cf_report$composite_scores
sec_cf_report$loadings
#3
sec_cf_report$paths$coefficients
sec_cf_report$paths$pvalues

# boot_cf_pls <- bootstrap_model(sec_cf_pls, nboot= 1000, cores = 4, seed = NULL)
# boot_cf_pls_report <- summary(boot_cf_pls)
# tvalues <- boot_cf_pls_report$bootstrapped_paths[, "T Stat."]
# df = nrow(sec)
# pvalues <- round( pt(tvalues, df, lower.tail = FALSE), 3)
# data.frame(pvalues)


