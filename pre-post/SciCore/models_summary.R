###
# study name: TIMCI
# program name: models_summary.R 
# program purpose: program that exports datasets for modelling and merged overall dataset for analysis
# author: Silvia Cicconi  
###


library(gee)
library(emmeans)

source("~/timci_prepost_models_po/functions.R")
load(file="~/timci_prepost_models_po/models_po.RData")
load(file="~/timci_prepost_models_po/models_so.RData")



##PO
##2-59 months

mod.sum <- c("Urgent referrals", rep("", 7))
mod.sum <- rbind(mod.sum,c("Primary analysis", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.un)[1,c(3,4)], summMod(ref.mod,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.ke.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.se.un)[1,c(3,4)], c("NE","")))

mod.sum <- rbind(mod.sum,c("Sensitivity analysis-first encounters", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.first.un)[1,c(3,4)], summMod(ref.mod.first,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke" & d0_mod$first_enc==1), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.ke.first.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se" & d0_mod$first_enc==1), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.se.first.un)[1,c(3,4)], c("NE","")))

mod.sum <- rbind(mod.sum,c("Sensitivity analysis-referral caregiver or registry", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)), c("pre_post", "urg_ref_sa1")])[1:2],  summMod(ref.mod.sa1.un)[1,c(3,4)], summMod(ref.mod.sa1,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "urg_ref_sa1")])[1:2],  summMod(ref.mod.ke.sa1.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "urg_ref_sa1")])[1:2],  summMod(ref.mod.se.sa1.un)[1,c(3,4)], c("NE","")))

mod.sum <- rbind(mod.sum, c("Sensitivity analysis-referral caregiver and registry", "Combined", rep("-", 6)))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "urg_ref_sa2")])[1:2],  summMod(ref.mod.ke.sa2.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Senegal", rep("-", 6)))

mod.sum <- rbind(mod.sum,c("Sensitivity analysis-pre-post overlapping period of the year", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1)), c("pre_post_sa1", "urg_ref")])[1:2],  summMod(ref.mod.pp.sa1.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1)  & d0_mod$country=="ke"), c("pre_post_sa1", "urg_ref")])[1:2],  summMod(ref.mod.ke.pp.sa1.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1) &  d0_mod$country=="se"), c("pre_post_sa1", "urg_ref")])[1:2],  summMod(ref.mod.se.pp.sa1.un)[1,c(3,4)], c("NE","")))

mod.sum <- rbind(mod.sum,c("Sensitivity analysis-pre-post based on conutry-specific key dates", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2)), c("pre_post_sa2", "urg_ref")])[1:2],  summMod(ref.mod.pp.sa2.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2)  & d0_mod$country=="ke"), c("pre_post_sa2", "urg_ref")])[1:2],  summMod(ref.mod.ke.pp.sa2.un)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2) &  d0_mod$country=="se"), c("pre_post_sa2", "urg_ref")])[1:2],  summMod(ref.mod.se.pp.sa2.un)[1,c(3,4)], c("NE","")))

mod.sum <- rbind(mod.sum,c("Subgroup-location rural", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$category_fac==0), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.loc0.un, lim=T)[1,c(3,4)], c("NE","-")))
mod.sum <- rbind(mod.sum,c("Subgroup-location urban", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$category_fac==1), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.loc1.un, lim=T)[1,c(3,4)], summMod(ref.mod.loc1.un, lim=T)[1,c(3,4)]))


mod.sum <- rbind(mod.sum, c("Antibiotics prescription", rep("", 7)))
mod.sum <- rbind(mod.sum,c("Primary analysis", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) ), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.un)[1,c(3,4)], summMod(anti.mod,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.ke.un)[1,c(3,4)], summMod(anti.mod.ke,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.se.un)[1,c(3,4)], summMod(anti.mod.se,lim=T)[1,c(3,4)]))

mod.sum <- rbind(mod.sum,c("Sensitivity analysis-first encounters", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.first.un)[1,c(3,4)], summMod(anti.mod.first,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke" & d0_mod$first_enc==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.ke.first.un)[1,c(3,4)], summMod(anti.mod.ke.first,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se" & d0_mod$first_enc==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.se.first.un)[1,c(3,4)], summMod(anti.mod.se.first,lim=T)[1,c(3,4)]))

mod.sum <- rbind(mod.sum,c("Sensitivity analysis-pre-post overlapping period of the year", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1)), c("pre_post_sa1", "antibio_flag")])[1:2],  summMod(anti.mod.pp.sa1.un)[1,c(3,4)],  summMod(anti.mod.pp.sa1,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1)  & d0_mod$country=="ke"), c("pre_post_sa1", "antibio_flag")])[1:2],  summMod(anti.mod.ke.pp.sa1.un)[1,c(3,4)], summMod(anti.mod.ke.pp.sa1,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1) &  d0_mod$country=="se"), c("pre_post_sa1", "antibio_flag")])[1:2],  summMod(anti.mod.se.pp.sa1.un)[1,c(3,4)], summMod(anti.mod.se.pp.sa1,lim=T)[1,c(3,4)]))

mod.sum <- rbind(mod.sum,c("Sensitivity analysis-pre-post based on conutry-specific key dates", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2)), c("pre_post_sa2", "antibio_flag")])[1:2],  summMod(anti.mod.pp.sa2.un)[1,c(3,4)], summMod(anti.mod.pp.sa2,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2)  & d0_mod$country=="ke"), c("pre_post_sa2", "antibio_flag")])[1:2],  summMod(anti.mod.ke.pp.sa2.un)[1,c(3,4)], summMod(anti.mod.ke.pp.sa2,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2) &  d0_mod$country=="se"), c("pre_post_sa2", "antibio_flag")])[1:2],  summMod(anti.mod.se.pp.sa2.un)[1,c(3,4)], summMod(anti.mod.se.pp.sa2,lim=T)[1,c(3,4)]))

mod.sum <- rbind(mod.sum,c("Subgroup-age 2 to 12 months", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$age_mod==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.age1.un,lim=T)[1,c(3,4)], summMod(anti.mod.age1,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum,c("Subgroup-age 13 to 59 months", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$age_mod==2), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.age2.un,lim=T)[1,c(3,4)], summMod(anti.mod.age2,lim=T)[1,c(3,4)]))

mod.sum <- rbind(mod.sum,c("Subgroup-sex male", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.sex1.un,lim=T)[1,c(3,4)], summMod(anti.mod.sex1,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum,c("Subgroup-sex female", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==2), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.sex2.un,lim=T)[1,c(3,4)], summMod(anti.mod.sex2,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum,c("Subgroup-sex unknown", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==99), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.sex99.un,lim=T)[1,c(3,4)], c("NE","-")))

mod.sum <- rbind(mod.sum,c("Subgroup-without cough or difficulty breathing", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==0), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.cb0.un,lim=T)[1,c(3,4)], summMod(anti.mod.cb0,lim=T)[1,c(3,4)]))
mod.sum <- rbind(mod.sum,c("Subgroup-with cough or difficulty breathing", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.cb1.un,lim=T)[1,c(3,4)], summMod(anti.mod.cb1,lim=T)[1,c(3,4)]))


## Format and add col names to summary tables
mod.sum <- as.data.frame(mod.sum)
names(mod.sum) <- c("Outcome", "Analysis", "N (%) Pre-intervention", "N (%) Post-intervention", "Unadjusted", "p-value", "Adjusted", "p-value")


##SO
##2-59 months

mod.sum.so <- c("Severe complications by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) ), c("pre_post", "sev_comp_d7")])[1:2],  summMod(sc.mod.un)[1,c(3,4)], c("NE",""))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "sev_comp_d7")])[1:2],  summMod(sc.mod.ke.un)[1,c(3,4)], c("NE","")))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "sev_comp_d7")])[1:2],  summMod(sc.mod.se.un)[1,c(3,4)], c("NE","")))

mod.sum.so <- rbind(mod.sum.so, c("Appropriate referrals", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)), c("pre_post", "pr_hos")])[1:2],  summMod(ar.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "pr_hos")])[1:2],  summMod(ar.mod.ke.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "pr_hos")])[1:2],   summMod(ar.mod.se.un)[1,c(3,4)], c("NE","-")))

mod.sum.so <- rbind(mod.sum.so, c("Completed referrals by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) ), c("pre_post", "comp_ref_d7")])[1:2],  summMod(cref.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "comp_ref_d7")])[1:2],  summMod(cref.mod.ke.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "comp_ref_d7")])[1:2],  summMod(cref.mod.se.un)[1,c(3,4)], c("NE","-")))

mod.sum.so <- rbind(mod.sum.so,c("Referrals with non-severe disease", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) ), c("pre_post", "ref_non_sev")])[1:2],  summMod(ref.ns.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "ref_non_sev")])[1:2],  summMod(ref.ns.mod.ke.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "ref_non_sev")])[1:2],  summMod(ref.ns.mod.se.un)[1,c(3,4)], c("NE","-")))

mod.sum.so <- rbind(mod.sum.so,c("Cure rate by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) ), c("pre_post", "cured_d7")])[1:2],  summMod(cure.mod.un)[1,c(3,4)], summMod(cure.mod,lim=T)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "cured_d7")])[1:2],  summMod(cure.mod.ke.un)[1,c(3,4)], summMod(cure.mod.ke,lim=T)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "cured_d7")])[1:2],  summMod(cure.mod.se.un)[1,c(3,4)], summMod(cure.mod.se,lim=T)[1,c(3,4)]))

mod.sum.so <- rbind(mod.sum.so,c("Febrile children tested for malaria", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$fev_flag==1), c("pre_post", "mal_flag")])[1:2],  summMod(test.mod.un)[1,c(3,4)], summMod(test.mod,lim=T)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"& d0_mod$fev_flag==1), c("pre_post", "mal_flag")])[1:2],  summMod(test.mod.ke.un)[1,c(3,4)], summMod(test.mod.ke,lim=T)[1,c(3,4)]))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"& d0_mod$fev_flag==1), c("pre_post", "mal_flag")])[1:2],  summMod(test.mod.se.un)[1,c(3,4)], summMod(test.mod.se,lim=T)[1,c(3,4)]))

mod.sum.so <- rbind(mod.sum.so, c("Antimalarial prescription for children tested positive", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==1 & d0_mod$malaria_res==2), c("pre_post", "antimal_flag")])[1:2],  summMod(mpos.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="ke"& d0_mod$mal_flag==1 & d0_mod$malaria_res==2), c("pre_post", "antimal_flag")])[1:2],  summMod(mpos.mod.ke.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"& d0_mod$mal_flag==1 & d0_mod$malaria_res==2), c("pre_post", "antimal_flag")])[1:2],  c("NE","-"), c("NE","-")))

mod.sum.so <- rbind(mod.sum.so,c("Antimalarial prescription for children tested negative", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==1 & d0_mod$malaria_res==1), c("pre_post", "antimal_flag")])[1:2],  summMod(mneg.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="ke"& d0_mod$mal_flag==1 & d0_mod$malaria_res==1), c("pre_post", "antimal_flag")])[1:2],   summMod(mneg.mod.ke.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"& d0_mod$mal_flag==1 & d0_mod$malaria_res==1), c("pre_post", "antimal_flag")])[1:2],  c("NE","-"), c("NE","-")))

mod.sum.so <- rbind(mod.sum.so,c("Antimalarial prescription for children untested", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==0), c("pre_post", "antimal_flag")])[1:2],  summMod(unt.mod.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="ke"& d0_mod$mal_flag==0), c("pre_post", "antimal_flag")])[1:2],  summMod(unt.mod.ke.un)[1,c(3,4)], c("NE","-")))
mod.sum.so <- rbind(mod.sum.so, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"& d0_mod$mal_flag==0), c("pre_post", "antimal_flag")])[1:2],  c("NE","-"), c("NE","-")))

## Format and add col names to summary tables
mod.sum.so <- as.data.frame(mod.sum.so)
names(mod.sum.so) <- c("Outcome", "Analysis","N (%) Pre-intervention", "N (%) Post-intervention", "Unadjusted", "p-value", "Adjusted", "p-value")



##PO
##0-2 months
mod.sum_yi <- c("Urgent referrals", rep("", 7))
mod.sum_yi <- rbind(mod.sum_yi,c("Primary analysis", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & !is.na(d0_mod$pre_post)), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.ke.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.se.un_yg)[1,c(3,4)], c("NE","")))

mod.sum_yi <- rbind(mod.sum_yi,c("Sensitivity analysis-first encounters", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.first.un.yg)[1,c(3,4)],  c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke" & d0_mod$first_enc==1), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.ke.first.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se" & d0_mod$first_enc==1), c("pre_post", "urg_ref")])[1:2],  summMod(ref.mod.se.first.un.yg)[1,c(3,4)], c("NE","")))

mod.sum_yi <- rbind(mod.sum_yi,c("Sensitivity analysis-referral caregiver or registry", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)), c("pre_post", "urg_ref_sa1")])[1:2],  summMod(ref.mod.sa1.un.yg)[1,c(3,4)],  c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "urg_ref_sa1")])[1:2],  summMod(ref.mod.ke.sa1.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "urg_ref_sa1")])[1:2],  summMod(ref.mod.se.sa1.un.yg)[1,c(3,4)], c("NE","")))

mod.sum_yi <- rbind(mod.sum_yi, c("Sensitivity analysis-referral caregiver and registry", "Combined", rep("-", 6)))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "urg_ref_sa2")])[1:2],  summMod(ref.mod.ke.sa2.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", rep("-", 6)))

mod.sum_yi <- rbind(mod.sum_yi,c("Sensitivity analysis-pre-post overlapping period of the year", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1)), c("pre_post_sa1", "urg_ref")])[1:2],  summMod(ref.mod.pp.sa1.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1)  & d0_mod$country=="ke"), c("pre_post_sa1", "urg_ref")])[1:2],  summMod(ref.mod.ke.pp.sa1.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1) &  d0_mod$country=="se"), c("pre_post_sa1", "urg_ref")])[1:2],  summMod(ref.mod.se.pp.sa1.un.yg)[1,c(3,4)], c("NE","")))

mod.sum_yi <- rbind(mod.sum_yi,c("Sensitivity analysis-pre-post based on conutry-specific key dates", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2)), c("pre_post_sa2", "urg_ref")])[1:2],  summMod(ref.mod.pp.sa2.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2)  & d0_mod$country=="ke"), c("pre_post_sa2", "urg_ref")])[1:2],  summMod(ref.mod.ke.pp.sa2.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2) &  d0_mod$country=="se"), c("pre_post_sa2", "urg_ref")])[1:2],  summMod(ref.mod.se.pp.sa2.un.yg)[1,c(3,4)], c("NE","")))



mod.sum_yi <- rbind(mod.sum_yi, c("Antibiotics prescription", rep("", 7)))
mod.sum_yi <- rbind(mod.sum_yi,c("Primary analysis", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.un_yg)[1,c(3,4)], summMod(anti.mod_yg,lim=T)[1,c(3,4)]))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.ke.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.se.un_yg)[1,c(3,4)], c("NE","-")))

mod.sum_yi <- rbind(mod.sum_yi,c("Sensitivity analysis-first encounters", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.first.un.yg)[1,c(3,4)],  summMod(anti.mod.first.yg,lim=T)[1,c(3,4)]))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke" & d0_mod$first_enc==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.ke.first.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se" & d0_mod$first_enc==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.se.first.un.yg)[1,c(3,4)], c("NE","")))

mod.sum_yi <- rbind(mod.sum_yi,c("Sensitivity analysis-pre-post overlapping period of the year", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1)), c("pre_post_sa1", "antibio_flag")])[1:2],  summMod(anti.mod.pp.sa1.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1)  & d0_mod$country=="ke"), c("pre_post_sa1", "antibio_flag")])[1:2],  summMod(anti.mod.ke.pp.sa1.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1) &  d0_mod$country=="se"), c("pre_post_sa1", "antibio_flag")])[1:2],  summMod(anti.mod.se.pp.sa1.un.yg)[1,c(3,4)], c("NE","")))

mod.sum_yi <- rbind(mod.sum_yi,c("Sensitivity analysis-pre-post based on conutry-specific key dates", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2)), c("pre_post_sa2", "antibio_flag")])[1:2],  summMod(anti.mod.pp.sa2.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2)  & d0_mod$country=="ke"), c("pre_post_sa2", "antibio_flag")])[1:2],  summMod(anti.mod.ke.pp.sa2.un.yg)[1,c(3,4)], c("NE","-")))
mod.sum_yi <- rbind(mod.sum_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2) &  d0_mod$country=="se"), c("pre_post_sa2", "antibio_flag")])[1:2],  summMod(anti.mod.se.pp.sa2.un.yg)[1,c(3,4)], c("NE","")))

mod.sum_yi <- rbind(mod.sum_yi,c("Subgroup-without cough or difficulty breathing", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==0), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.cb0.un.yg)[1,c(3,4)], summMod(anti.mod.cb0.yg,lim=T)[1,c(3,4)]))
mod.sum_yi <- rbind(mod.sum_yi,c("Subgroup-with cough or difficulty breathing", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==1), c("pre_post", "antibio_flag")])[1:2],  summMod(anti.mod.cb1.un.yg)[1,c(3,4)], c("NE","")))


## Format and add col names to summary tables
mod.sum_yi <- as.data.frame(mod.sum_yi)
names(mod.sum_yi) <- c("Outcome", "Analysis", "N (%) Pre-intervention", "N (%) Post-intervention", "Unadjusted", "p-value", "Adjusted", "p-value")


##SO
##0-2 months

mod.sum.so_yi <- c("Severe complications by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ), c("pre_post", "sev_comp_d7")])[1:2],  summMod(sc.mod.un_yg)[1,c(3,4)], c("NE","-"))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "sev_comp_d7")])[1:2],  summMod(sc.mod.ke.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "sev_comp_d7")])[1:2],  summMod(sc.mod.se.un_yg)[1,c(3,4)], c("NE","-")))

mod.sum.so_yi <- rbind(mod.sum.so_yi, c("Appropriate referrals", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)), c("pre_post", "pr_hos")])[1:2],  summMod(ar.mod.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "pr_hos")])[1:2],  summMod(ar.mod.ke.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "pr_hos")])[1:2],   summMod(ar.mod.se.un_yg)[1,c(3,4)], c("NE","-")))

mod.sum.so_yi <- rbind(mod.sum.so_yi, c("Completed referrals by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ), c("pre_post", "comp_ref_d7")])[1:2],  summMod(cref.mod.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "comp_ref_d7")])[1:2],  summMod(cref.mod.ke.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "comp_ref_d7")])[1:2],  summMod(cref.mod.se.un_yg)[1,c(3,4)], c("NE","-")))

mod.sum.so_yi <- rbind(mod.sum.so_yi,c("Referrals with non-severe disease", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ), c("pre_post", "ref_non_sev")])[1:2],  summMod(ref.ns.mod.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "ref_non_sev")])[1:2],  c("NE","-"), c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "ref_non_sev")])[1:2],  summMod(ref.ns.mod.se.un_yg)[1,c(3,4)], c("NE","-")))

mod.sum.so_yi <- rbind(mod.sum.so_yi,c("Cure rate by Day7", "Combined", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ), c("pre_post", "cured_d7")])[1:2],  summMod(cure.mod.un_yg)[1,c(3,4)], summMod(cure.mod_yg,lim=T)[1,c(3,4)]))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Kenya", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)  & d0_mod$country=="ke"), c("pre_post", "cured_d7")])[1:2],  summMod(cure.mod.ke.un_yg)[1,c(3,4)], c("NE","-")))
mod.sum.so_yi <- rbind(mod.sum.so_yi, c("","Senegal", mod_sum(d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) &  d0_mod$country=="se"), c("pre_post", "cured_d7")])[1:2],  summMod(cure.mod.se.un_yg)[1,c(3,4)], c("NE","-")))



## Format and add col names to summary tables
mod.sum.so_yi <- as.data.frame(mod.sum.so_yi)
names(mod.sum.so_yi) <- c("Outcome", "Analysis", "N (%) Pre-intervention", "N (%) Post-intervention", "Unadjusted", "p-value", "Adjusted", "p-value")


save(mod.sum, mod.sum_yi, mod.sum.so, mod.sum.so_yi, file="~/timci_prepost_models_so/models_summary.RData")
