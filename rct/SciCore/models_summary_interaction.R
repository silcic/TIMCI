###
# study name: TIMCI
# program name: models_summary_interaction.R 
# program purpose: program that summarises interaction models to assess subgroup analyses (if p<0.2 as per SAP)
# author: Silvia Cicconi  
###

#load(file=file.path(paste0(dir_gen_dat, "/models/models_summary.RData")))

library(gee)
library(emmeans)

source("~/timci_rct_models_po/functions.R")
load(file="~/timci_rct_models_po/d0_mod.RData")
load(file="~/timci_rct_models_po/models_po.RData")
load(file="~/timci_rct_models_po/models_so.RData")

sc.mod.age.sum <- summary(sc.mod.age); formatC(round((2 * (1 - pnorm(abs(sc.mod.age.sum$coef[,"Robust z"])))),3), format="f", 3) #0.877
sc.mod.sex.sum <- summary(sc.mod.sex); formatC(round((2 * (1 - pnorm(abs(sc.mod.sex.sum$coef[,"Robust z"])))),3), format="f", 3) #0.739
sc.mod.dia.sum <- summary(sc.mod.dia); formatC(round((2 * (1 - pnorm(abs(sc.mod.dia.sum$coef[,"Robust z"])))),3), format="f", 3) #0.104
sc.mod.cb.sum <- summary(sc.mod.cb); formatC(round((2 * (1 - pnorm(abs(sc.mod.cb.sum$coef[,"Robust z"])))),3), format="f", 3) #0.365
sc.mod.ref.sum <- summary(sc.mod.ref); formatC(round((2 * (1 - pnorm(abs(sc.mod.ref.sum$coef[,"Robust z"])))),3), format="f", 3) #0.961
sc.mod.aa.sum <- summary(sc.mod.aa); formatC(round((2 * (1 - pnorm(abs(sc.mod.aa.sum$coef[,"Robust z"])))),3), format="f", 3) #0.530
sc.mod.dis.sum <- summary(sc.mod.dis); formatC(round((2 * (1 - pnorm(abs(sc.mod.dis.sum$coef[,"Robust z"])))),3), format="f", 3) #0.673
sc.mod.ty.sum <- summary(sc.mod.ty); formatC(round((2 * (1 - pnorm(abs(sc.mod.ty.sum$coef[,"Robust z"])))),3), format="f", 3) #0.885
 


save.image(file="~/timci_rct_models_po/models_summary_interaction.RData")


