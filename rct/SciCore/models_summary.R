###
# study name: TIMCI
# program name: models_summary.R 
# program purpose: program that exports datasets for modelling and merged overall dataset for analysis
# author: Silvia Cicconi  
###

library(gee)
library(emmeans)


load(file="~/timci_rct_models_po/models_po.RData")
load(file="~/timci_rct_models_po/models_so.RData")
source("~/timci_rct_models_po/functions.R")


##PO
##2-59 months

mod.sum <- c("Severe complications by Day7", rep("", 8))
mod.sum <- rbind(mod.sum, c("Primary analysis ITT", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 ), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.un)[1,c(3,4)], summMod(sc.mod)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.in.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.tz.un)[1,c(3,4)], summMod(sc.mod.tz)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[3], summMod(sc.mod.tz.un)[2,c(3,4)], summMod(sc.mod.tz)[2,c(3,4)]))

mod.sum <- rbind(mod.sum, c("Primary analysis CC", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.un.cc)[1,c(3,4)], summMod(sc.mod.cc)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1 & d0_mod$country=="in"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.in.un.cc)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1 &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.tz.un.cc)[1,c(3,4)], summMod(sc.mod.tz.cc)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1  & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[3], summMod(sc.mod.tz.un.cc)[2,c(3,4)], summMod(sc.mod.tz.cc)[2,c(3,4)]))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-first encounters", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.sa.first.un)[1,c(3,4)], summMod(sc.mod.sa.first)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1 & d0_mod$country=="in"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.sa.first.in.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1 &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.sa.first.tz.un)[1,c(3,4)], summMod(sc.mod.sa.first.tz)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1  & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[3], summMod(sc.mod.sa.first.tz.un)[2,c(3,4)], summMod(sc.mod.sa.first.tz)[2,c(3,4)]))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-hosp cut-off Day0", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  ), c("intervention", "sev_comp_d7_sa1")])[1:2], "-", summMod(sc.mod.sa1.un)[1,c(3,4)], summMod(sc.mod.sa1)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="in"), c("intervention", "sev_comp_d7_sa1")])[1:2], "-", summMod(sc.mod.sa1.in.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa1")])[1:2], "-", summMod(sc.mod.sa1.tz.un)[1,c(3,4)], summMod(sc.mod.sa1.tz)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa1")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa1")])[3], summMod(sc.mod.sa1.tz.un)[2,c(3,4)], summMod(sc.mod.sa1.tz)[2,c(3,4)]))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-hosp cut-off Day3", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  ), c("intervention", "sev_comp_d7_sa2")])[1:2], "-", summMod(sc.mod.sa2.un)[1,c(3,4)], summMod(sc.mod.sa2)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="in"), c("intervention", "sev_comp_d7_sa2")])[1:2], "-", summMod(sc.mod.sa2.in.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa2")])[1:2], "-", summMod(sc.mod.sa2.tz.un)[1,c(3,4)], summMod(sc.mod.sa2.tz)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa2")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa2")])[3], summMod(sc.mod.sa2.tz.un)[2,c(3,4)], summMod(sc.mod.sa2.tz)[2,c(3,4)]))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-hosp cut-off Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  ), c("intervention", "sev_comp_d7_sa3")])[1:2], "-", summMod(sc.mod.sa3.un)[1,c(3,4)], summMod(sc.mod.sa3)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="in"), c("intervention", "sev_comp_d7_sa3")])[1:2], "-", summMod(sc.mod.sa3.in.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa3")])[1:2], "-", summMod(sc.mod.sa3.tz.un)[1,c(3,4)], summMod(sc.mod.sa3.tz)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa3")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa3")])[3], summMod(sc.mod.sa3.tz.un)[2,c(3,4)], summMod(sc.mod.sa3.tz)[2,c(3,4)]))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-referral caregiver and registry", "Combined", rep("-", 7)))
mod.sum <- rbind(mod.sum, c("","India", rep("-", 7)))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref1")])[1:2], "-", summMod(sc.mod.sa.ref1.tz.un)[1,c(3,4)], summMod(sc.mod.sa.ref1.tz)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref1")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref1")])[3], summMod(sc.mod.sa.ref1.tz.un)[2,c(3,4)], summMod(sc.mod.sa.ref1.tz)[2,c(3,4)]))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-referral only registry", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  ), c("intervention", "sev_comp_d7_sa_ref2")])[1:2], "-", summMod(sc.mod.sa.ref2.un)[1,c(3,4)], summMod(sc.mod.sa.ref2)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="in"), c("intervention", "sev_comp_d7_sa_ref2")])[1:2], "-", summMod(sc.mod.sa.ref2.in.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref2")])[1:2], "-", summMod(sc.mod.sa.ref2.tz.un)[1,c(3,4)], summMod(sc.mod.sa.ref2.tz)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref2")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref2")])[3], summMod(sc.mod.sa.ref2.tz.un)[2,c(3,4)], summMod(sc.mod.sa.ref2.tz)[2,c(3,4)]))

mod.sum <- rbind(mod.sum, c("Subgroup analysis - no severe diagnosis", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$severe_diag==0), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.dia.n.un)[1,c(3,4)], summMod(sc.mod.dia.n)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("Subgroup analysis - severe diagnosis", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$severe_diag==1), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.dia.y.un)[1,c(3,4)], summMod(sc.mod.dia.y)[1,c(3,4)]))


mod.sum <- rbind(mod.sum, c("Appropriate referrals", rep("", 8)))
mod.sum <- rbind(mod.sum, c("Primary analysis ITT", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0), c("intervention", "pr_hos")])[1:2], "-", summMod(ar.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"), c("intervention", "pr_hos")])[1:2], "-", summMod(ar.mod.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "pr_hos")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "pr_hos")])[3], summMod(ar.mod.tz.un)[2,c(3,4)], c("NE","-")))

mod.sum <- rbind(mod.sum, c("Primary analysis CC", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1), c("intervention", "pr_hos")])[1:2], "-", summMod(ar.mod.un.cc)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1 & d0_mod$country=="in"), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1 &  d0_mod$country=="tz"), c("intervention", "pr_hos")])[1:2], "-", summMod(ar.mod.tz.un.cc)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1 & d0_mod$country=="tz"), c("intervention", "pr_hos")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$cc==1  & d0_mod$country=="tz"), c("intervention", "pr_hos")])[3], summMod(ar.mod.tz.un.cc)[2,c(3,4)], c("NE","-")))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-first encounters", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1), c("intervention", "pr_hos")])[1:2], "-", summMod(ar.mod.sa.first.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1 & d0_mod$country=="in"), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1 &  d0_mod$country=="tz"), c("intervention", "pr_hos")])[1:2], "-", summMod(ar.mod.sa.first.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1 & d0_mod$country=="tz"), c("intervention", "pr_hos")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1  & d0_mod$country=="tz"), c("intervention", "pr_hos")])[3], summMod(ar.mod.sa.first.tz.un)[2,c(3,4)], c("NE","-")))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-hosp cut-off Day0", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  ), c("intervention", "pr_hos_sa1")])[1:2], "-", summMod(ar.mod.sa1.un)[1,c(3,4)],c("NE","-")))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="in"), c("intervention", "pr_hos_sa1")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "pr_hos_sa1")])[1:2], "-", summMod(ar.mod.sa1.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "pr_hos_sa1")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "pr_hos_sa1")])[3], summMod(ar.mod.sa1.tz.un)[2,c(3,4)], c("NE","-")))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-hosp cut-off Day3", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  ), c("intervention", "pr_hos_sa2")])[1:2], "-", summMod(ar.mod.sa2.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="in"), c("intervention", "pr_hos_sa2")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "pr_hos_sa2")])[1:2], "-", summMod(ar.mod.sa2.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "pr_hos_sa2")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "pr_hos_sa2")])[3], summMod(ar.mod.sa2.tz.un)[2,c(3,4)],c("NE","-")))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-hosp cut-off Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  ), c("intervention", "pr_hos_sa3")])[1:2], "-",c("NE","-"),c("NE","-")))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="in"), c("intervention", "pr_hos_sa3")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "pr_hos_sa3")])[1:2], "-", summMod(ar.mod.sa3.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "pr_hos_sa3")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "pr_hos_sa3")])[3], summMod(ar.mod.sa3.tz.un)[2,c(3,4)], c("NE","-")))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-referral caregiver and registry", "Combined", rep("-", 7)))
mod.sum <- rbind(mod.sum, c("","India", rep("-", 7)))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "pr_hos_sa_ref1")])[1:2], "-", summMod(ar.mod.sa.ref1.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "pr_hos_sa_ref1")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "pr_hos_sa_ref1")])[3], summMod(ar.mod.sa.ref1.tz.un)[2,c(3,4)], c("NE","-")))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-referral only registry", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0  ), c("intervention", "pr_hos_sa_ref2")])[1:2], "-", summMod(ar.mod.sa.ref2.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="in"), c("intervention", "pr_hos_sa_ref2")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0   &  d0_mod$country=="tz"), c("intervention", "pr_hos_sa_ref2")])[1:2], "-", summMod(ar.mod.sa.ref2.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0   & d0_mod$country=="tz"), c("intervention", "pr_hos_sa_ref2")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0    & d0_mod$country=="tz"), c("intervention", "pr_hos_sa_ref2")])[3], summMod(ar.mod.sa.ref2.tz.un)[2,c(3,4)], c("NE","-")))





## Format and add col names to summary tables
mod.sum <- as.data.frame(mod.sum)
names(mod.sum) <- c("Outcome", "Analysis", "N (%) Control", "N (%) PO", "N (%) PO+CDSA", "Univariate", "p-value", "Multivariate", "p-value")


##SO
##2-59 months

mod.sum.so <- c("Severe complications by Day28", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 ), c("intervention", "sev_comp_d28")])[1:2], "-", summMod(sc28.mod.un)[1,c(3,4)], summMod(sc28.mod)[1,c(3,4)])
mod.sum.so <- rbind(mod.sum.so, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"), c("intervention", "sev_comp_d28")])[1:2], "-", summMod(sc28.mod.in.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"), c("intervention", "sev_comp_d28")])[1:2], "-", summMod(sc28.mod.tz.un)[1,c(3,4)], summMod(sc28.mod.tz)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "sev_comp_d28")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "sev_comp_d28")])[3], summMod(sc28.mod.tz.un)[2,c(3,4)], summMod(sc28.mod.tz)[2,c(3,4)]))

mod.sum.so <- rbind(mod.sum.so,c("Urgent referrals", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 ), c("intervention", "urg_ref")])[1:2], "-", summMod(ref.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"), c("intervention", "urg_ref")])[1:2], "-", summMod(ref.mod.in.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"), c("intervention", "urg_ref")])[1:2], "-", summMod(ref.mod.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "urg_ref")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "urg_ref")])[3], summMod(ref.mod.tz.un)[2,c(3,4)], c("NE","-")))

mod.sum.so <- rbind(mod.sum.so, c("Completed referrals by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 ), c("intervention", "comp_ref_d7")])[1:2], "-", summMod(cref.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"), c("intervention", "comp_ref_d7")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"), c("intervention", "comp_ref_d7")])[1:2], "-", summMod(cref.mod.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "comp_ref_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "comp_ref_d7")])[3], summMod(cref.mod.tz.un)[2,c(3,4)], c("NE","-")))

mod.sum.so <- rbind(mod.sum.so,c("Referrals with non-severe disease", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 ), c("intervention", "ref_non_sev")])[1:2], "-", summMod(ref.ns.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"), c("intervention", "ref_non_sev")])[1:2], "-", summMod(ref.ns.mod.in.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"), c("intervention", "ref_non_sev")])[1:2], "-", summMod(ref.ns.mod.tz.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "ref_non_sev")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "ref_non_sev")])[3], summMod(ref.ns.mod.tz.un)[2,c(3,4)], c("NE","-")))

mod.sum.so <- rbind(mod.sum.so,c("Cure rate by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 ), c("intervention", "cured_d7")])[1:2], "-", summMod(cure.mod.un)[1,c(3,4)], summMod(cure.mod)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"), c("intervention", "cured_d7")])[1:2], "-", summMod(cure.mod.in.un)[1,c(3,4)], summMod(cure.mod.in)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"), c("intervention", "cured_d7")])[1:2], "-", summMod(cure.mod.tz.un)[1,c(3,4)], summMod(cure.mod.tz)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "cured_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "cured_d7")])[3], summMod(cure.mod.tz.un)[2,c(3,4)], summMod(cure.mod.tz)[2,c(3,4)]))

mod.sum.so <- rbind(mod.sum.so,c("Antibiotics prescription", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 ), c("intervention", "antibio_flag")])[1:2], "-", summMod(antib.mod.un)[1,c(3,4)], summMod(antib.mod)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"), c("intervention", "antibio_flag")])[1:2], "-", summMod(antib.mod.in.un)[1,c(3,4)], summMod(antib.mod.in)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"), c("intervention", "antibio_flag")])[1:2], "-", summMod(antib.mod.tz.un)[1,c(3,4)], summMod(antib.mod.tz)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "antibio_flag")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"), c("intervention", "antibio_flag")])[3], summMod(antib.mod.tz.un)[2,c(3,4)], summMod(antib.mod.tz)[2,c(3,4)]))

mod.sum.so <- rbind(mod.sum.so,c("Febrile children tested for malaria", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$fev_flag==1), c("intervention", "mal_flag")])[1:2], "-", summMod(test.mod.un)[1,c(3,4)], summMod(test.mod)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==0  & d0_mod$country=="in"& d0_mod$fev_flag==1), c("intervention", "mal_flag")])[1:2], "-", summMod(test.mod.in.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"& d0_mod$fev_flag==1), c("intervention", "mal_flag")])[1:2], "-", summMod(test.mod.tz.un)[1,c(3,4)], summMod(test.mod.tz)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"& d0_mod$fev_flag==1), c("intervention", "mal_flag")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"& d0_mod$fev_flag==1), c("intervention", "mal_flag")])[3], summMod(test.mod.tz.un)[2,c(3,4)], summMod(test.mod.tz)[2,c(3,4)]))

mod.sum.so <- rbind(mod.sum.so, c("Antimalarial prescription for children tested positive", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$mal_flag==1 & d0_mod$malaria_res==1), c("intervention", "antimal_flag")])[1:2], "-", summMod(mpos.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","India", rep("-", 7)))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"& d0_mod$mal_flag==1 & d0_mod$malaria_res==1), c("intervention", "antimal_flag")])[1:2], "-", summMod(mpos.mod.tz.un)[1,c(3,4)], summMod(mpos.mod.tz)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"& d0_mod$mal_flag==1 & d0_mod$malaria_res==1), c("intervention", "antimal_flag")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==1 & d0_mod$malaria_res==1), c("intervention", "antimal_flag")])[3], summMod(mpos.mod.tz.un)[2,c(3,4)], summMod(mpos.mod.tz.un)[2,c(3,4)]))

mod.sum.so <- rbind(mod.sum.so,c("Antimalarial prescription for children tested negative", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$mal_flag==1 & d0_mod$malaria_res==0), c("intervention", "antimal_flag")])[1:2], "-", summMod(mneg.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","India", rep("-", 7)))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"& d0_mod$mal_flag==1 & d0_mod$malaria_res==0), c("intervention", "antimal_flag")])[1:2], "-", summMod(mneg.mod.tz.un)[1,c(3,4)], summMod(mneg.mod.tz)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"& d0_mod$mal_flag==1 & d0_mod$malaria_res==0), c("intervention", "antimal_flag")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==1 & d0_mod$malaria_res==0), c("intervention", "antimal_flag")])[3], summMod(mneg.mod.tz.un)[2,c(3,4)], summMod(mneg.mod.tz)[2,c(3,4)]))

mod.sum.so <- rbind(mod.sum.so,c("Antimalarial prescription for children untested", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$mal_flag==0), c("intervention", "antimal_flag")])[1:2], "-", summMod(unt.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","India", rep("-", 7)))
mod.sum.so <- rbind(mod.sum.so, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==0 &  d0_mod$country=="tz"& d0_mod$mal_flag==0), c("intervention", "antimal_flag")])[1:2], "-", summMod(unt.mod.tz.un)[1,c(3,4)], summMod(unt.mod.tz)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"& d0_mod$mal_flag==0), c("intervention", "antimal_flag")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==0), c("intervention", "antimal_flag")])[3], summMod(unt.mod.tz.un)[2,c(3,4)], summMod(unt.mod.tz)[2,c(3,4)]))

## Format and add col names to summary tables
mod.sum.so <- as.data.frame(mod.sum.so)
names(mod.sum.so) <- c("Outcome", "Analysis", "N (%) Control", "N (%) PO", "N (%) PO+CDSA", "Unadjusted", "p-value", "Adjusted", "p-value")



##PO
##0-2 months
mod.sum_yi <- c("Severe complications by Day7", rep("", 8))
mod.sum_yi <- rbind(mod.sum_yi, c("Primary analysis ITT", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 ), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.in.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1 &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.tz.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[3], summMod(sc.mod.tz.un_yg)[2,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Primary analysis CC", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.un.cc_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1 & d0_mod$country=="in"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.in.un.cc_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1 &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.tz.un.cc_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1  & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[3], summMod(sc.mod.tz.un.cc_yg)[2,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Sensitivity analysis-first encounters", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$first_enc==1), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.sa.first.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$first_enc==1 & d0_mod$country=="in"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.sa.first.in.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$first_enc==1 &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc.mod.sa.first.tz.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$first_enc==1 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$first_enc==1  & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[3], summMod(sc.mod.sa.first.tz.un_yg)[2,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Sensitivity analysis-hosp cut-off Day0", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1), c("intervention", "sev_comp_d7_sa1")])[1:2], "-", summMod(sc.mod.sa1.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "sev_comp_d7_sa1")])[1:2], "-", summMod(sc.mod.sa1.in.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1  &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa1")])[1:2], "-", summMod(sc.mod.sa1.tz.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa1")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa1")])[3], summMod(sc.mod.sa1.tz.un_yg)[2,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Sensitivity analysis-hosp cut-off Day3", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 ), c("intervention", "sev_comp_d7_sa2")])[1:2], "-", summMod(sc.mod.sa2.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "sev_comp_d7_sa2")])[1:2], "-", summMod(sc.mod.sa2.in.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1  &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa2")])[1:2], "-", summMod(sc.mod.sa2.tz.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa2")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa2")])[3], summMod(sc.mod.sa2.tz.un_yg)[2,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Sensitivity analysis-hosp cut-off Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 ), c("intervention", "sev_comp_d7_sa3")])[1:2], "-", summMod(sc.mod.sa3.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "sev_comp_d7_sa3")])[1:2], "-", summMod(sc.mod.sa3.in.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1  &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa3")])[1:2], "-", summMod(sc.mod.sa3.tz.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa3")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa3")])[3], summMod(sc.mod.sa3.tz.un_yg)[2,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Sensitivity analysis-referral caregiver and registry", "Combined", rep("-", 7)))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", rep("-", 7)))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1  &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref1")])[1:2], "-", summMod(sc.mod.sa.ref1.tz.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref1")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref1")])[3], summMod(sc.mod.sa.ref1.tz.un_yg)[2,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Sensitivity analysis-referral only registry", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 ), c("intervention", "sev_comp_d7_sa_ref2")])[1:2], "-", summMod(sc.mod.sa.ref2.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "sev_comp_d7_sa_ref2")])[1:2], "-", summMod(sc.mod.sa.ref2.in.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1  &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref2")])[1:2], "-", summMod(sc.mod.sa.ref2.tz.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref2")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1   & d0_mod$country=="tz"), c("intervention", "sev_comp_d7_sa_ref2")])[3], summMod(sc.mod.sa.ref2.tz.un_yg)[2,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Appropriate referrals", rep("", 8)))
mod.sum_yi <- rbind(mod.sum_yi, c("Primary analysis ITT", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1 &  d0_mod$country=="tz"), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "pr_hos")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "pr_hos")])[3], c("NE","-"), c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi, c("Primary analysis CC", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1 & d0_mod$country=="in"), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1 &  d0_mod$country=="tz"), c("intervention", "pr_hos")])[1:2], "-", c("NE","-"), c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1 & d0_mod$country=="tz"), c("intervention", "pr_hos")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$cc==1  & d0_mod$country=="tz"), c("intervention", "pr_hos")])[3], c("NE","-"), c("NE","-")))

# Format and add col names to summary tables
mod.sum_yi <- as.data.frame(mod.sum_yi)
names(mod.sum_yi) <- c("Outcome", "Analysis", "N (%) Control", "N (%) PO", "N (%) PO+CDSA", "Unadjusted", "p-value", "Adjusted", "p-value")




##SO
##0-2 months months
mod.sum.so_yi <- c("Severe complications by Day28", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1), c("intervention", "sev_comp_d28")])[1:2], "-", summMod(sc28.mod.un_yi)[1,c(3,4)], c("NE","-"))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc28.mod.in.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1 &  d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1:2], "-", summMod(sc28.mod.tz.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "sev_comp_d7")])[3], summMod(sc28.mod.tz.un_yi)[2,c(3,4)], c("NE","-")))

mod.sum.so_yi <- rbind(mod.sum.so_yi, c("Urgent referrals", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 ), c("intervention", "urg_ref")])[1:2], "-", summMod(ref.mod.un_yi)[1,c(3,4)], summMod(ref.mod_yi)[1,c(3,4)]))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "urg_ref")])[1:2], "-", summMod(ref.mod.in.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1 &  d0_mod$country=="tz"), c("intervention", "urg_ref")])[1:2], "-", summMod(ref.mod.tz.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "urg_ref")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "urg_ref")])[3], summMod(ref.mod.tz.un_yi)[2,c(3,4)], c("NE","-")))

mod.sum.so_yi <- rbind(mod.sum.so_yi, c("Completed referrals", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 ), c("intervention", "comp_ref_d7")])[1:2], "-", summMod(cref.mod.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "comp_ref_d7")])[1:2], "-", summMod(cref.mod.in.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1 &  d0_mod$country=="tz"), c("intervention", "comp_ref_d7")])[1:2], "-", summMod(cref.mod.tz.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "comp_ref_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "comp_ref_d7")])[3], summMod(cref.mod.tz.un_yi)[2,c(3,4)], c("NE","-")))

mod.sum.so_yi <- rbind(mod.sum.so_yi,c("Referrals with non-severe disease", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 ), c("intervention", "ref_non_sev")])[1:2], "-", summMod(ref.ns.mod.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "ref_non_sev")])[1:2], "-", summMod(ref.ns.mod.in.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1 &  d0_mod$country=="tz"), c("intervention", "ref_non_sev")])[1:2], "-", summMod(ref.ns.mod.tz.un_yi)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "ref_non_sev")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "ref_non_sev")])[3], summMod(ref.ns.mod.tz.un_yi)[2,c(3,4)],c("NE","-")))

mod.sum.so_yi <- rbind(mod.sum.so_yi,c("Cure rate by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 ), c("intervention", "cured_d7")])[1:2], "-", summMod(cure.mod.un_yi)[1,c(3,4)], summMod(cure.mod_yi)[1,c(3,4)]))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "cured_d7")])[1:2], "-", summMod(cure.mod.in.un_yi)[1,c(3,4)], summMod(cure.mod.in_yi)[1,c(3,4)]))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1 &  d0_mod$country=="tz"), c("intervention", "cured_d7")])[1:2], "-", summMod(cure.mod.tz.un_yi)[1,c(3,4)], summMod(cure.mod.tz_yi)[1,c(3,4)]))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "cured_d7")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "cured_d7")])[3], summMod(cure.mod.tz.un_yi)[2,c(3,4)], summMod(cure.mod.tz_yi)[2,c(3,4)]))

mod.sum.so_yi <- rbind(mod.sum.so_yi,c("Antibiotics prescription", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1), c("intervention", "antibio_flag")])[1:2], "-", summMod(antib.mod.un_yi)[1,c(3,4)], summMod(antib.mod_yi)[1,c(3,4)]))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","India", mod_sum(d0_mod[which(d0_mod$yg_infant==1  & d0_mod$country=="in"), c("intervention", "antibio_flag")])[1:2], "-", summMod(antib.mod.in.un_yi)[1,c(3,4)], summMod(antib.mod.in_yi)[1,c(3,4)]))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Tanzania", mod_sum(d0_mod[which(d0_mod$yg_infant==1 &  d0_mod$country=="tz"), c("intervention", "antibio_flag")])[1:2], "-", summMod(antib.mod.tz.un_yi)[1,c(3,4)], summMod(antib.mod.tz_yi)[1,c(3,4)]))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "antibio_flag")])[1], "-", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"), c("intervention", "antibio_flag")])[3], summMod(antib.mod.tz.un_yi)[2,c(3,4)], summMod(antib.mod.tz_yi)[2,c(3,4)]))



## Format and add col names to summary tables
mod.sum.so_yi <- as.data.frame(mod.sum.so_yi)
names(mod.sum.so_yi) <- c("Outcome", "Analysis", "N (%) Control", "N (%) PO", "N (%) PO+CDSA", "Unadjusted", "p-value", "Adjusted", "p-value")



save(mod.sum, mod.sum_yi, mod.sum.so, mod.sum.so_yi, file="~/timci_rct_models_po/models_summary.RData")
