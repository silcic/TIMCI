###
# study name: TIMCI
# program name: models_summary_interaction.R 
# program purpose: program that summarises interaction models to assess subgroup analyses (if p<0.2 as per SAP)
# author: Silvia Cicconi  
###

#load(file=file.path(paste0(dir_gen_dat, "/models/models_summary.RData")))
 
library(gee)
library(emmeans)

source("~/timci_prepost_models_po/functions.R")
load(file="~/timci_prepost_models_po/d0_mod.RData")
load(file="~/timci_prepost_models_po/models_po.RData")
load(file="~/timci_prepost_models_po/models_so.RData")

ref.mod.age.sum <- summary(ref.mod.age); formatC(round((2 * (1 - pnorm(abs(ref.mod.age.sum$coef[,"Robust z"])))),3), format="f", 3) #0.897
ref.mod.cb.sum <- summary(ref.mod.cb); formatC(round((2 * (1 - pnorm(abs(ref.mod.cb.sum$coef[,"Robust z"])))),3), format="f", 3) #0.484
ref.mod.loc.sum <- summary(ref.mod.loc); formatC(round((2 * (1 - pnorm(abs(ref.mod.loc.sum$coef[,"Robust z"])))),3), format="f", 3) #0.061

anti.mod.age.sum <- summary(anti.mod.age); formatC(round((2 * (1 - pnorm(abs(anti.mod.age.sum$coef[,"Robust z"])))),3), format="f", 3) #<0.001
anti.mod.sex.sum <- summary(anti.mod.sex); formatC(round((2 * (1 - pnorm(abs(anti.mod.sex.sum$coef[,"Robust z"])))),3), format="f", 3) #0.180
anti.mod.cb.sum <- summary(anti.mod.cb); formatC(round((2 * (1 - pnorm(abs(anti.mod.cb.sum$coef[,"Robust z"])))),3), format="f", 3) #<0.001
anti.mod.loc.sum <- summary(anti.mod.loc); formatC(round((2 * (1 - pnorm(abs(anti.mod.loc.sum$coef[,"Robust z"])))),3), format="f", 3) #0.865

anti.mod.cb.yg.sum <- summary(anti.mod.cb.yg); formatC(round((2 * (1 - pnorm(abs(anti.mod.cb.yg.sum$coef[,"Robust z"])))),3), format="f", 3) #0.008
anti.mod.loc.yg.sum <- summary(anti.mod.loc.yg); formatC(round((2 * (1 - pnorm(abs(anti.mod.loc.yg.sum$coef[,"Robust z"])))),3), format="f", 3) #0.996


save.image(file="~/timci_prepost_models_po/models_summary_interaction.RData")
