###
# study name: TIMCI
# program name: models_so.Rmd  
# program purpose: Secondary outcomes modelling 
# author: Silvia Cicconi  
###



##### Load packages and data
library(gee)
library(emmeans)

load("~/timci_rct_models_so/d0_mod.RData")


#### Format and order
d0_mod$fid <- as.numeric(as.factor(d0_mod$fid))
d0_mod <- d0_mod[order(d0_mod$country, d0_mod$fid),]



##################################
### Cure rate
##################################

#### 2-59 months
#unadjusted
cure.mod.un <- gee(cured_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
cure.mod.in.un <- gee(cured_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
cure.mod.tz.un <- gee(cured_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#adjusted
cure.mod <- gee(cured_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
cure.mod.in <- gee(cured_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
cure.mod.tz <- gee(cured_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#### 0-2 months
#unadjusted
cure.mod.un_yi <- gee(cured_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.in.un_yi <- gee(cured_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.tz.un_yi <- gee(cured_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#adjusted
cure.mod_yi <- gee(cured_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.in_yi <- gee(cured_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.tz_yi <- gee(cured_d7 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


##################################
### Antibiotics
##################################

#### 2-59 months
#unadjusted
antib.mod.un <- gee(antibio_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
antib.mod.in.un <- gee(antibio_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
antib.mod.tz.un <- gee(antibio_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#adjusted
antib.mod <- gee(antibio_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
antib.mod.in <- gee(antibio_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
antib.mod.tz <- gee(antibio_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#### 0-2 months
#unadjusted
antib.mod.un_yi <- gee(antibio_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
antib.mod.in.un_yi <- gee(antibio_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
antib.mod.tz.un_yi <- gee(antibio_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#adjusted
antib.mod_yi <- gee(antibio_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
antib.mod.in_yi <- gee(antibio_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable") 
antib.mod.tz_yi <- gee(antibio_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


##################################
### Severe complications by Day28
##################################

#### 2-59 months
#unadjusted
sc28.mod.un <- gee(sev_comp_d28 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
sc28.mod.in.un <- gee(sev_comp_d28 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
sc28.mod.tz.un <- gee(sev_comp_d28 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#adjusted
sc28.mod <- gee(sev_comp_d28 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
#sc28.mod.in <- gee(sev_comp_d28 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
sc28.mod.tz <- gee(sev_comp_d28 ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#### 0-2 months
#unadjusted
sc28.mod.un_yi <- gee(sev_comp_d28 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc28.mod.in.un_yi <- gee(sev_comp_d28 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc28.mod.tz.un_yi <- gee(sev_comp_d28 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")



##################################
### Febrile children tested for malaria
##################################

#### 2-59 months
#unadjusted
test.mod.un <- gee(mal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
test.mod.in.un <- gee(mal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in" & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
test.mod.tz.un <- gee(mal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#adjusted
test.mod <- gee(mal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
#test.mod.in <- gee(mal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(main_cg_der) + factor(trans_time_der) + factor(pre_care_trt) + factor(danger_sign) + factor(sx_difficulty_breath), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in" & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
test.mod.tz <- gee(mal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")



##################################
### Malaria prescription for positive
##################################

#### 2-59 months
#unadjusted
mpos.mod.un <- gee(antimal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
mpos.mod.tz.un <- gee(antimal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")


#adjusted
mpos.mod <- gee(antimal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
mpos.mod.tz <- gee(antimal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")



##################################
### Malaria prescription for negative
##################################

#### 2-59 months
#unadjusted
mneg.mod.un <- gee(antimal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$mal_flag==1 & d0_mod$malaria_res==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
mneg.mod.tz.un <- gee(antimal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==1 & d0_mod$malaria_res==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")


#adjusted
mneg.mod <- gee(antimal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$mal_flag==1 & d0_mod$malaria_res==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
mneg.mod.tz <- gee(antimal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==1 & d0_mod$malaria_res==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")




##################################
### Malaria prescription for untested
##################################

#### 2-59 months
#unadjusted
unt.mod.un <- gee(antimal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
unt.mod.tz.un <- gee(antimal_flag ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")


#adjusted
unt.mod <- gee(antimal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
unt.mod.tz <- gee(antimal_flag ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")




##################################
### Urgent referrals
##################################
 
#### 2-59 months
#unadjusted
ref.mod.un <- gee(urg_ref ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
ref.mod.in.un <- gee(urg_ref ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
ref.mod.tz.un <- gee(urg_ref ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
 
 
#adjusted
#ref.mod <- gee(urg_ref ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
#ref.mod.in <- gee(urg_ref ~ factor(intervention) + factor(category) + factor(type_der) + factor(trans_time_der) + factor(pre_care_trt) + factor(sx_lethargy) + factor(sx_lethargy), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
#ref.mod.tz <- gee(urg_ref ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#### 0-2 months
#unadjusted
ref.mod.un_yi <- gee(urg_ref ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.in.un_yi <- gee(urg_ref ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.tz.un_yi <- gee(urg_ref ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#adjusted
ref.mod_yi <- gee(urg_ref ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")



##################################
### Completed referral
##################################

#### 2-59 months
#unadjusted
cref.mod.un <- gee(comp_ref_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
#cref.mod.in.un <- gee(comp_ref_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
cref.mod.tz.un <- gee(comp_ref_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")


#### 0-2 months
#unadjusted
cref.mod.un_yi <- gee(comp_ref_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cref.mod.in.un_yi <- gee(comp_ref_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cref.mod.tz.un_yi <- gee(comp_ref_d7 ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


##################################
### Referral with non-severe disease
##################################

#### 2-59 months
#unadjusted
ref.ns.mod.un <- gee(ref_non_sev  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
ref.ns.mod.in.un <- gee(ref_non_sev  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
ref.ns.mod.tz.un <- gee(ref_non_sev  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#adjusted
#ref.ns.mod <- gee(ref_non_sev ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
#ref.ns.mod.tz <- gee(ref_non_sev ~ factor(intervention) + factor(category) + factor(type_der) + factor(trans_time_der) + factor(pre_care_trt) + factor(sx_lethargy) + factor(sx_lethargy), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")
#ref.ns.mod.tz <- gee(ref_non_sev ~ factor(intervention) + factor(district) + factor(type_der) + factor(pre_care_trt), data = d0_mod[which(d0_mod$yg_infant==0 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable"); save.image("~/timci_rct_models_so/models_so.RData")

#### 0-2 months
#unadjusted
ref.ns.mod.un_yi <- gee(ref_non_sev  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.ns.mod.in.un_yi <- gee(ref_non_sev  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$intervention!=2 & d0_mod$country=="in"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.ns.mod.tz.un_yi <- gee(ref_non_sev  ~ factor(intervention), data = d0_mod[which(d0_mod$yg_infant==1 & d0_mod$country=="tz"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

save.image("~/timci_rct_models_so/models_so.RData")

