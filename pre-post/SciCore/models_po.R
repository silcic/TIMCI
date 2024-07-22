###
# study name: TIMCI
# program name: models_po.R  
# program purpose: Primary outcomes modelling 
# author: Silvia Cicconi  
###


##### Load packages and data
library(gee)
library(emmeans)

load("~/timci_prepost_models_po/d0_mod.RData")


#### Format and order
d0_mod$fid <- as.numeric(as.factor(d0_mod$fid))
d0_mod <- d0_mod[order(d0_mod$country, d0_mod$fid),]


#re-format levels
d0_mod$sx_fever <- factor(d0_mod$sx_fever, c(0,1,98))
d0_mod$sx_cough <- factor(d0_mod$sx_cough, c(0,1,98))
d0_mod$sx_diarrhoea <- factor(d0_mod$sx_diarrhoea, c(0,1,98))



##################################
### Referrals
##################################


#### 2-59 months

#### Primary analysis models - unadjusted
ref.mod.un <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.un <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.un <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
  
#### Primary analysis models - adjusted
ref.mod <- gee(urg_ref ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.mod.ke <- gee(urg_ref ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.mod.se <- gee(urg_ref ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Sensitivity analysis - first encounters
ref.mod.first.un <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.first.un <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.first.un <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.first <- gee(urg_ref ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - urgent referral at least one between registry and caregiver
ref.mod.sa1.un <- gee(urg_ref_sa1 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.sa1.un <- gee(urg_ref_sa1 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.sa1.un <- gee(urg_ref_sa1 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.sa1 <- gee(urg_ref_sa1 ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - urgent referral in both registry and caregiver
ref.mod.ke.sa2.un <- gee(urg_ref_sa2 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.mod.ke.sa2 <- gee(urg_ref_sa2 ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - overlapping periods of the year
ref.mod.pp.sa1.un <- gee(urg_ref ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.pp.sa1.un <- gee(urg_ref ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.pp.sa1.un <- gee(urg_ref ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.mod.pp.sa1 <- gee(urg_ref ~ factor(pre_post_sa1) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - key dates
ref.mod.pp.sa2.un <- gee(urg_ref ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.pp.sa2.un <- gee(urg_ref ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.pp.sa2.un <- gee(urg_ref ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#ref.mod.pp.sa2 <- gee(urg_ref ~ factor(pre_post_sa2) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Interaction age
ref.mod.age <- gee(urg_ref ~ factor(pre_post)*factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Interaction sex
#ref.mod.sex <- gee(urg_ref ~ factor(pre_post)*factor(sex) + factor(age_mod) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Interaction cough/difficult breathing (remove specific variable for cough)
ref.mod.cb <- gee(urg_ref ~ factor(pre_post)*factor(cough_breath) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Interaction facility location
ref.mod.loc <- gee(urg_ref ~ factor(pre_post)*factor(category_fac) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

ref.mod.loc0.un <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$category_fac==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.loc1.un <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$category_fac==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#ref.mod.loc0 <- gee(urg_ref ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$category_fac==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.loc1 <- gee(urg_ref ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$category_fac==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")



#### 0-2 months

#### Primary analysis models - unadjusted
ref.mod.un_yg <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.un_yg <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.un_yg <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Primary analysis models - adjusted (not enough numbers)


#### Sensitivity analysis - first encounters
ref.mod.first.un.yg <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.first.un.yg <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.first.un.yg <- gee(urg_ref ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Sensitivity analysis - urgent referral at least one between registry and caregiver
ref.mod.sa1.un.yg <- gee(urg_ref_sa1 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.sa1.un.yg <- gee(urg_ref_sa1 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.sa1.un.yg <- gee(urg_ref_sa1 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Sensitivity analysis - urgent referral in both registry and caregiver
ref.mod.ke.sa2.un.yg <- gee(urg_ref_sa2 ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Sensitivity analysis - overlapping periods of the year
ref.mod.pp.sa1.un.yg <- gee(urg_ref ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.pp.sa1.un.yg <- gee(urg_ref ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.pp.sa1.un.yg <- gee(urg_ref ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Sensitivity analysis - key dates
ref.mod.pp.sa2.un.yg <- gee(urg_ref ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.ke.pp.sa2.un.yg <- gee(urg_ref ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
ref.mod.se.pp.sa2.un.yg <- gee(urg_ref ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")





















##########################
### Antibiotics
##########################


#### 2-59 months

##Primary analysis models - unadjusted
anti.mod.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

##Primary analysis models - adjusted
anti.mod <- gee(antibio_flag ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke <- gee(antibio_flag ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se <- gee(antibio_flag ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Sensitivity analysis - first encounters
anti.mod.first.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.first.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.first.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.first <- gee(antibio_flag ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.first <- gee(antibio_flag ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.first <- gee(antibio_flag ~ factor(pre_post) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - overlapping periods of the year
anti.mod.pp.sa1.un <- gee(antibio_flag ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.pp.sa1.un <- gee(antibio_flag ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.pp.sa1.un <- gee(antibio_flag ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.pp.sa1 <- gee(antibio_flag ~ factor(pre_post_sa1) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.pp.sa1 <- gee(antibio_flag ~ factor(pre_post_sa1) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.pp.sa1 <- gee(antibio_flag ~ factor(pre_post_sa1) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - key dates
anti.mod.pp.sa2.un <- gee(antibio_flag ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.pp.sa2.un <- gee(antibio_flag ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.pp.sa2.un <- gee(antibio_flag ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.pp.sa2 <- gee(antibio_flag ~ factor(pre_post_sa2) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.pp.sa2 <- gee(antibio_flag ~ factor(pre_post_sa2) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.pp.sa2 <- gee(antibio_flag ~ factor(pre_post_sa2) + factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Interaction age
anti.mod.age <- gee(antibio_flag ~ factor(pre_post)*factor(age_mod)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.age1.un <- gee(antibio_flag ~ factor(pre_post) , data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$age_mod==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.age2.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$age_mod==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.age1 <- gee(antibio_flag ~ factor(pre_post)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$age_mod==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.age2 <- gee(antibio_flag ~ factor(pre_post)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$age_mod==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Interaction sex
anti.mod.sex <- gee(antibio_flag ~ factor(pre_post)*factor(sex) + factor(age_mod) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.sex1.un <- gee(antibio_flag ~ factor(pre_post) , data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.sex2.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.sex99.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==99),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.sex1 <- gee(antibio_flag ~ factor(pre_post) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.sex2 <- gee(antibio_flag ~ factor(pre_post) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==2),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#anti.mod.sex99 <- gee(antibio_flag ~ factor(pre_post) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$sex==99),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Interaction cough/difficult breathing (remove specific variable for cough)
anti.mod.cb <- gee(antibio_flag ~ factor(pre_post)*factor(cough_breath) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.cb0.un <- gee(antibio_flag ~ factor(pre_post) , data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.cb1.un <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.cb0 <- gee(antibio_flag ~ factor(pre_post)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.cb1 <- gee(antibio_flag ~ factor(pre_post)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Interaction facility location
anti.mod.loc <- gee(antibio_flag ~ factor(pre_post)*factor(category_fac) + factor(age_mod) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==0 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")




#### 0-2 months


#### Primary analysis models - unadjusted
anti.mod.un_yg <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.un_yg <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.un_yg <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Primary analysis models - adjusted
anti.mod_yg <- gee(antibio_flag ~ factor(pre_post) +  factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#anti.mod.ke_yg <- gee(antibio_flag ~ factor(pre_post)   + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#anti.mod.se_yg <- gee(antibio_flag ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - first encounters
anti.mod.first.un.yg <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.first.un.yg <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="ke" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.first.un.yg <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$country=="se" & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.first.yg <- gee(antibio_flag ~ factor(pre_post) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$first_enc==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - overlapping periods of the year
anti.mod.pp.sa1.un.yg <- gee(antibio_flag ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.pp.sa1.un.yg <- gee(antibio_flag ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.pp.sa1.un.yg <- gee(antibio_flag ~ factor(pre_post_sa1), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#anti.mod.pp.sa1.yg <- gee(antibio_flag ~ factor(pre_post_sa1)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa1)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Sensitivity analysis - key dates
anti.mod.pp.sa2.un.yg <- gee(antibio_flag ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.ke.pp.sa2.un.yg <- gee(antibio_flag ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="ke"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.se.pp.sa2.un.yg <- gee(antibio_flag ~ factor(pre_post_sa2), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2) & d0_mod$country=="se"),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#anti.mod.pp.sa2.yg <- gee(antibio_flag ~ factor(pre_post_sa2)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post_sa2)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Interaction sex
#anti.mod.sex.yg <- gee(antibio_flag ~ factor(pre_post)*factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

#### Interaction cough/difficult breathing (remove specific variable for cough)
anti.mod.cb.yg <- gee(antibio_flag ~ factor(pre_post)*factor(cough_breath)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.cb0.un.yg <- gee(antibio_flag ~ factor(pre_post) , data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
anti.mod.cb1.un.yg <- gee(antibio_flag ~ factor(pre_post), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")

anti.mod.cb0.yg <- gee(antibio_flag ~ factor(pre_post)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==0),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")
#anti.mod.cb1.yg <- gee(antibio_flag ~ factor(pre_post)  + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post) & d0_mod$cough_breath==1),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")


#### Interaction facility location
anti.mod.loc.yg <- gee(antibio_flag ~ factor(pre_post)*factor(category_fac) + factor(sex) + factor(trans_time_der) + factor(sick_duration_mod)  + factor(pre_care_trt) +  factor(sx_cough) + factor(sx_fever) + factor(sx_diarrhoea) + factor(danger_sign), data = d0_mod[which(d0_mod$yg_infant==1 & !is.na(d0_mod$pre_post)),], family=(binomial(link="logit")), id=fid, corstr="exchangeable")




save.image("~/timci_prepost_models_po/models_po.RData")




