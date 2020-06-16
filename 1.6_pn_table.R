library(tidyverse)

rm(list=ls())
gc()

p_coef <- read_delim("coef/dk95_p_coef.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
p_coef$p250_c[p_coef$p250_p>0.05] <- 0
p_coef$p500_c[p_coef$p500_p>0.05] <- 0
p_coef$p1000_c[p_coef$p1000_p>0.05] <- 0
n_coef <- read_delim("coef/dk95_n_coef.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
n_coef$n250_c[n_coef$n250_p>0.05] <- 0
n_coef$n500_c[n_coef$n500_p>0.05] <- 0
n_coef$n1000_c[n_coef$n1000_p>0.05] <- 0

pn_coef <- p_coef[,c(1:4,6,8)] %>%
  inner_join(n_coef[,c(3:4,6,8)], by = "index")

pn_coef$p_change <- "Stable"
pn_coef$p_change[pn_coef$p500_c<0] <- "Decrease"
pn_coef$p_change[pn_coef$p500_c>0] <- "Increase"
pn_coef$n_change <- "Stable"
pn_coef$n_change[pn_coef$n500_c<0] <- "Decrease"
pn_coef$n_change[pn_coef$n500_c>0] <- "Increase"

pn_coef$pn_change <- "DD"
pn_coef$pn_change[pn_coef$p_change=="Decrease" & pn_coef$n_change == "Stable"] <- "DS"
pn_coef$pn_change[pn_coef$p_change=="Decrease" & pn_coef$n_change == "Increase"] <- "DI"
pn_coef$pn_change[pn_coef$p_change=="Stable" & pn_coef$n_change == "Decrease"] <- "SD"
pn_coef$pn_change[pn_coef$p_change=="Stable" & pn_coef$n_change == "Stable"] <- "SS"
pn_coef$pn_change[pn_coef$p_change=="Stable" & pn_coef$n_change == "Increase"] <- "SI"
pn_coef$pn_change[pn_coef$p_change=="Increase" & pn_coef$n_change == "Decrease"] <- "ID"
pn_coef$pn_change[pn_coef$p_change=="Increase" & pn_coef$n_change == "Stable"] <- "IS"
pn_coef$pn_change[pn_coef$p_change=="Increase" & pn_coef$n_change == "Increase"] <- "II"
table(pn_coef$pn_change)/length(pn_coef$pn_change)

write.table(pn_coef, file = "pn_coef", sep = "\t", row.names = F)