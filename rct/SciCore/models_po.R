###
# study name: TIMCI
# program name: models_po.Rmd  
# program purpose: Primary outcomes modelling children 2-59 months
# author: Silvia Cicconi  
###


##### Load packages and data
library(gee)
library(emmeans)

load("~/timci_rct_models_po/d0_mod.RData")


#### Format and order
d0_mod$fid <- as.numeric(as.factor(d0_mod$fid))
d0_mod <- d0_mod[order(d0_mod$country, d0_mod$fid),]



##################################
### Severe complications by Day7
##################################


#### 2-59 months

##Primary analysis models ITT - unadjusted
sc.mod.un <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
sc.mod.in.un <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
sc.mod.tz.un <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")

##Primary analysis models ITT - adjusted for baseline imbalance and stratification factors
sc.mod <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
#sc.mod.in <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
sc.mod.tz <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")

##Primary analysis models CC- unadjusted
sc.mod.un.cc <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$cc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
sc.mod.in.un.cc <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$cc==1 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
sc.mod.tz.un.cc <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$cc==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")

##Primary analysis models CC - adjusted for baseline imbalance and stratification factors
sc.mod.cc <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$cc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
#sc.mod.in.cc <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in" & d0_mod$cc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
sc.mod.tz.cc <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$cc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")

##Sensitivity analysis - first encounters
#unadjusted
sc.mod.sa.first.un <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.first.in.un <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$first_enc==1 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.first.tz.un <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$first_enc==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#adjusted
sc.mod.sa.first <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#sc.mod.sa.first.in <- gee(sev_comp_d7 ~  factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$first_enc==1 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.first.tz <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0  & d0_mod$first_enc==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - hosp cut-off day0
#unadjusted
sc.mod.sa1.un <- gee(sev_comp_d7_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa1.in.un <- gee(sev_comp_d7_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa1.tz.un <- gee(sev_comp_d7_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#adjusted
sc.mod.sa1 <- gee(sev_comp_d7_sa1  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#sc.mod.sa1.in <- gee(sev_comp_d7_sa1  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa1.tz <- gee(sev_comp_d7_sa1  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - hosp cut-off day3
#unadjusted
sc.mod.sa2.un <- gee(sev_comp_d7_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa2.in.un <- gee(sev_comp_d7_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa2.tz.un <- gee(sev_comp_d7_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#adjusted
sc.mod.sa2 <- gee(sev_comp_d7_sa2  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#sc.mod.sa2.in <- gee(sev_comp_d7_sa2  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa2.tz <- gee(sev_comp_d7_sa2  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - hosp cut-off day7
#unadjusted
sc.mod.sa3.un <- gee(sev_comp_d7_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa3.in.un <- gee(sev_comp_d7_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa3.tz.un <- gee(sev_comp_d7_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#adjusted
sc.mod.sa3 <- gee(sev_comp_d7_sa3  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#sc.mod.sa3.in <- gee(sev_comp_d7_sa3  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa3.tz <- gee(sev_comp_d7_sa3  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - urg ref 1
#unadjusted
sc.mod.sa.ref1.tz.un <- gee(sev_comp_d7_sa_ref1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#adjusted
sc.mod.sa.ref1.tz <- gee(sev_comp_d7_sa_ref1  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - urg ref 2
#unadjusted
sc.mod.sa.ref2.un <- gee(sev_comp_d7_sa_ref2 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.ref2.in.un <- gee(sev_comp_d7_sa_ref2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.ref2.tz.un <- gee(sev_comp_d7_sa_ref2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#adjusted
sc.mod.sa.ref2 <- gee(sev_comp_d7_sa_ref2  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#sc.mod.sa.ref2.in <- gee(sev_comp_d7_sa_ref2  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.ref2.tz <- gee(sev_comp_d7_sa_ref2  ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


#### Subgroups analysis
#interaction age
sc.mod.age <- gee(sev_comp_d7 ~ factor(intervention)*factor(age_mod) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),],family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#interaction sex
sc.mod.sex <- gee(sev_comp_d7 ~ factor(intervention)*factor(sex) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#interaction cough or difficult breathing
sc.mod.cb <- gee(sev_comp_d7 ~ factor(intervention)*factor(cough_breath) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#diagnosis (severe/non severe)
sc.mod.dia <- gee(sev_comp_d7 ~ factor(intervention)*factor(severe_diag) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

sc.mod.dia.n.un <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$severe_diag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.dia.n <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$severe_diag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

sc.mod.dia.y.un <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$severe_diag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.dia.y <- gee(sev_comp_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$severe_diag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#referral
sc.mod.ref <- gee(sev_comp_d7 ~ factor(intervention)*factor(urg_ref) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#antimicrobials (antimalarial or antibiotics)
sc.mod.aa <- gee(sev_comp_d7 ~ factor(intervention)*factor(antimicro_flag) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#stratification - factor(district)
sc.mod.dis <- gee(sev_comp_d7 ~ factor(intervention)*factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

#stratification - type
sc.mod.ty <- gee(sev_comp_d7 ~ factor(intervention)*factor(type_der) + factor(district) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")




#### 0-2 months

##Primary analysis models ITT - unadjusted
sc.mod.un_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc.mod.in.un_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc.mod.tz.un_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


##Primary analysis models CC- unadjusted
sc.mod.un.cc_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$cc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc.mod.in.un.cc_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$cc==1 & d0_mod$country=="in"),],  family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc.mod.tz.un.cc_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$cc==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


##Sensitivity analysis - first encounters
sc.mod.sa.first.un_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.first.in.un_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$first_enc==1 & d0$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.first.tz.un_yg <- gee(sev_comp_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$first_enc==1 & d0$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - hosp cut-off day0
sc.mod.sa1.un_yg <- gee(sev_comp_d7_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa1.in.un_yg <- gee(sev_comp_d7_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa1.tz.un_yg <- gee(sev_comp_d7_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


## Sensitivity analysis - hosp cut-off day3
sc.mod.sa2.un_yg <- gee(sev_comp_d7_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa2.in.un_yg <- gee(sev_comp_d7_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa2.tz.un_yg <- gee(sev_comp_d7_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - hosp cut-off day7
sc.mod.sa3.un_yg <- gee(sev_comp_d7_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa3.in.un_yg <- gee(sev_comp_d7_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa3.tz.un_yg <- gee(sev_comp_d7_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

##Sensitivity analysis - urg ref 1
sc.mod.sa.ref1.tz.un_yg <- gee(sev_comp_d7_sa_ref1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

##Sensitivity analysis - urg ref 2
sc.mod.sa.ref2.un_yg <- gee(sev_comp_d7_sa_ref2 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData") 
sc.mod.sa.ref2.in.un_yg <- gee(sev_comp_d7_sa_ref2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
sc.mod.sa.ref2.tz.un_yg <- gee(sev_comp_d7_sa_ref2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")





##########################
### Appropriate referral
##########################


#### 2-59 months

##Primary analysis models ITT - unadjusted
ar.mod.un <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
ar.mod.in.un <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
ar.mod.tz.un <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")


##Primary analysis models CC- unadjusted
ar.mod.un.cc <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$cc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
ar.mod.in.un.cc <- gee(pr_hos~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$cc==1 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")
ar.mod.tz.un.cc <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$cc==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/models_po.RData")


##Sensitivity analysis - first encounters 
#unadjusted
ar.mod.sa.first.un <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#ar.mod.sa.first.in.un <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$first_enc==1 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
ar.mod.sa.first.tz.un <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$first_enc==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - hosp cut-off day0
#unadjusted
ar.mod.sa1.un <- gee(pr_hos_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#ar.mod.sa1.in.un <- gee(pr_hos_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
ar.mod.sa1.tz.un <- gee(pr_hos_sa1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")

##Sensitivity analysis - hosp cut-off day3
#unadjusted
ar.mod.sa2.un <- gee(pr_hos_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#ar.mod.sa2.in.un <- gee(pr_hos_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
ar.mod.sa2.tz.un <- gee(pr_hos_sa2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - hosp cut-off day7
#unadjusted
ar.mod.sa3.un <- gee(pr_hos_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#ar.mod.sa3.in.un <- gee(pr_hos_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
ar.mod.sa3.tz.un <- gee(pr_hos_sa3  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - urg ref 1
#unadjusted
ar.mod.sa.ref1.tz.un <- gee(pr_hos_sa_ref1  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")


##Sensitivity analysis - urg ref 2
#unadjusted
ar.mod.sa.ref2.un <- gee(pr_hos_sa_ref2 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
#ar.mod.sa.ref2.in.un <- gee(pr_hos_sa_ref2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")
ar.mod.sa.ref2.tz.un <- gee(pr_hos_sa_ref2  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_po/mod_add.RData")





#### 0-2 months

##Primary analysis models ITT - unadjusted
#ar.mod.un_yg <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ar.mod.in.un_yg <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ar.mod.tz.un_yg <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


##Primary analysis models CC- unadjusted
#ar.mod.un.cc_yg <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$cc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ar.mod.in.un.cc_yg <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$cc==1 & d0_mod$country=="in"),],  family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ar.mod.tz.un.cc_yg <- gee(pr_hos ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$cc==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


save.image("~/timci_rct_models_po/models_po.RData")





