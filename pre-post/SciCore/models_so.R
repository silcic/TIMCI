###
# study name: TIMCI
# program name: models_so.R  
# program purpose: Secondary outcomes modelling children 2-59 months
# author: Silvia Cicconi  
###


##### Load packages and data
library(gee)
library(emmeans)

load("~/timci_prepost_models_so/d0_mod.RData")


#### Format and order
d0_mod$fid <- as.numeric(as.factor(d0_mod$fid))
d0_mod <- d0_mod[order(d0_mod$country, d0_mod$fid),]


#re-format levels
d0_mod$sx_fever <- factor(d0_mod$sx_fever, c(0,1,98))
d0_mod$sx_cough <- factor(d0_mod$sx_cough, c(0,1,98))
d0_mod$sx_diarrhoea <- factor(d0_mod$sx_diarrhoea, c(0,1,98))



##################################
### Cure rate
##################################

#### 2-59 months
#unadjusted
cure.mod.un <- gee(cured_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.ke.un <- gee(cured_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.se.un <- gee(cured_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#adjusted
cure.mod <- gee(cured_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.ke <- gee(cured_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.se <- gee(cured_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### 0-2 months
#unadjusted
cure.mod.un_yg <- gee(cured_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.ke.un_yg <- gee(cured_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cure.mod.se.un_yg <- gee(cured_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#adjusted
cure.mod_yg <- gee(cured_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#cure.mod.ke_yg <- gee(cured_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#cure.mod.se_yg <- gee(cured_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")



##################################
### Severe complications by Day7
##################################

#### 2-59 months
#unadjusted
sc.mod.un <- gee(sev_comp_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc.mod.ke.un <- gee(sev_comp_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc.mod.se.un <- gee(sev_comp_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#adjusted
#sc.mod <- gee(sev_comp_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#sc.mod.ke <- gee(sev_comp_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#sc.mod.se <- gee(sev_comp_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### 0-2 months
#unadjusted
sc.mod.un_yg <- gee(sev_comp_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc.mod.ke.un_yg <- gee(sev_comp_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
sc.mod.se.un_yg <- gee(sev_comp_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#adjusted
#sc.mod <- gee(sev_comp_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#sc.mod.ke <- gee(sev_comp_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#sc.mod.se <- gee(sev_comp_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


##################################
### Febrile children tested for malaria
##################################

#### 2-59 months
#unadjusted
test.mod.un <- gee(mal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
test.mod.ke.un <- gee(mal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
test.mod.se.un <- gee(mal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
test.mod <- gee(mal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
test.mod.ke <- gee(mal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
test.mod.se <- gee(mal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$fev_flag==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")



##################################
### Malaria prescription for positive
##################################

#### 2-59 months
#unadjusted
mpos.mod.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==1 & d0_mod$malaria_res==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
mpos.mod.ke.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$mal_flag==1 & d0_mod$malaria_res==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#mpos.mod.se.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#mpos.mod <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==1 & d0_mod$malaria_res==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#mpos.mod.ke <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$mal_flag==1 & d0_mod$malaria_res==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#mpos.mod.se <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$mal_flag==1 & d0_mod$malaria_res==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")




##################################
### Malaria prescription for negative
##################################

#### 2-59 months
#unadjusted
mneg.mod.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
mneg.mod.ke.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#mneg.mod.se.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#mneg.mod <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#mneg.mod.ke <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#mneg.mod.se <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$mal_flag==1 & d0_mod$malaria_res==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")



##################################
### Malaria prescription for untested
##################################

#### 2-59 months
#unadjusted
unt.mod.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
unt.mod.ke.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#unt.mod.se.un <- gee(antimal_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#unt.mod <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#unt.mod.ke <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#unt.mod.se <- gee(antimal_flag ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")



##################################
### Appropriate referrals
##################################

#### 2-59 months
ar.mod.un <- gee(pr_hos ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ar.mod.ke.un <- gee(pr_hos ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ar.mod.se.un <- gee(pr_hos ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#ar.mod <- gee(pr_hos ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ar.mod.ke <- gee(pr_hos ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ar.mod.se <- gee(pr_hos ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### 0-2 months
ar.mod.un_yg <- gee(pr_hos ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ar.mod.ke.un_yg <- gee(pr_hos ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ar.mod.se.un_yg <- gee(pr_hos ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#ar.mod <- gee(pr_hos ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ar.mod.ke <- gee(pr_hos ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ar.mod.se <- gee(pr_hos ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


##################################
### Completed referral
##################################

#### 2-59 months
#unadjusted
cref.mod.un <- gee(comp_ref_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cref.mod.ke.un <- gee(comp_ref_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cref.mod.se.un <- gee(comp_ref_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#cref.mod <- gee(comp_ref_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#cref.mod.ke <- gee(comp_ref_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#cref.mod.se <- gee(comp_ref_d7 ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$mal_flag==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### 0-2 months
#unadjusted
cref.mod.un_yg <- gee(comp_ref_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cref.mod.ke.un_yg <- gee(comp_ref_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
cref.mod.se.un_yg <- gee(comp_ref_d7 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#cref.mod <- gee(comp_ref_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#cref.mod.ke <- gee(comp_ref_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#cref.mod.se <- gee(comp_ref_d7 ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")




##################################
### Referral with non-severe disease
##################################

#### 2-59 months
ref.ns.mod.un <- gee(ref_non_sev ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.ns.mod.ke.un <- gee(ref_non_sev ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.ns.mod.se.un <- gee(ref_non_sev ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#ref.ns.mod <- gee(ref_non_sev ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.ns.mod.ke <- gee(ref_non_sev ~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.ns.mod.se <- gee(ref_non_sev~ factor(pre_post) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### 0-2 months
ref.ns.mod.un_yg <- gee(ref_non_sev ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.ns.mod.ke.un_yg <- gee(ref_non_sev ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.ns.mod.se.un_yg <- gee(ref_non_sev ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#adjusted
#ref.ns.mod <- gee(ref_non_sev ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.ns.mod.ke <- gee(ref_non_sev ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.ns.mod.se <- gee(ref_non_sev~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" ),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


save.image("~/timci_prepost_models_so/models_so.RData")

