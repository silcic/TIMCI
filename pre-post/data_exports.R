###
# study name: TIMCI
# program name: data_exports.R 
# program purpose: program that exports datasets for modelling and merged overall dataset for analysis
# author: Silvia Cicconi  
###


##dataset for modelling
d0_mod <- d0[,c("child_id", "fid", "country", "pre_post", "pre_post_sa1", "pre_post_sa2", "district", "type",  "category", "category_fac", "yg_infant", "first_enc",
                "age_mod", "sex", "pre_care_trt", "trans_time_der","sick_duration", "sick_duration_mod", "sx_cough", "sx_fever", "sx_diarrhoea", "danger_sign", "cough_breath", "cough_diarr_fev",  "pr_hos", 
                "sev_comp_d7", "urg_ref", "urg_ref_sa1", "urg_ref_sa2", "los", "comp_ref_d7", "ref_non_sev", "cured_d7", "sev_hyp", "hypo_o2_ref", "antibio_flag", "antibio_app", "antibio_unapp", 
                "single_diag", "antibio_req", "antibio_first_line", "fev_flag", "malaria_res", "mal_flag", "antimal_flag", "sc_fu_d7", "sc_fu_vis", "unsc_fu_d7", "unsc_fu_vis")]
save(d0_mod, file=file.path(paste(dir_gen_dat, "d0_mod.RData", sep="/")))
write.xlsx(d0_mod, file.path(paste(dir_gen_dat, "d0_mod.xlsx", sep="/")))
write.csv(d0_mod, file.path(paste(dir_gen_dat, "d0_mod.csv", sep="/")))



##select variables to be exported in the merged overall dataset
#facility characteristics, flags (cc, fu, etc.)
#primary outcomes
#secondary outcomes
d0_ke_ex <- merge(d0[which(d0$country=="ke"),c("child_id", "fid", "district", "type",  "category", "pre_post", "pre_post_sa1", "pre_post_sa2",
                                               "first_enc", "fu_flag", "date_call", "wth_flag_d7",
                                               "dth_flag", "date_death_day7", "dth_time_flag", 
                                               "hos_flag", "date_hosp_day7", "hos_time_flag", "pr_se_hos",
                                               "sev_comp_d7",
                                               "pr_hos", "pr_hos_sa1", "pr_hos_sa2", "pr_hos_sa3", 
                                               "urg_ref", "urg_ref_sa1","urg_ref_sa2", "los", "comp_ref_d7", "ref_non_sev", "cured_d7", "hypox_sum", "severe_diag", 
                                               "hypo_o2_ref", "antibio_flag", "antimal_flag", "sc_fu_d7", "sc_fu_vis", "unsc_fu_d7", "unsc_fu_vis")], d0[, vars_d0_ke[-which(vars_d0_ke%in%c("fid", "district"))]], by="child_id", all.x=T)
write.xlsx(d0_ke_ex, file.path(paste(dir_gen_dat, paste0("TIMCI_analysis_data_Kenya_", Sys.Date(), ".xlsx"), sep="/")))
write.csv(d0_ke_ex, file.path(paste(dir_gen_dat, paste0("TIMCI_analysis_data_Kenya_", Sys.Date(), ".csv"), sep="/")))



d0_se_ex <- merge(d0[which(d0$country=="se"),c("child_id", "fid", "district", "type",  "category", "pre_post", "pre_post_sa1", "pre_post_sa2",
                                               "first_enc", "fu_flag", "date_call", "wth_flag_d7",
                                               "dth_flag", "date_death_day7", "dth_time_flag", 
                                               "hos_flag", "date_hosp_day7", "hos_time_flag", "pr_se_hos",
                                               "sev_comp_d7",
                                               "pr_hos", "pr_hos_sa1", "pr_hos_sa2", "pr_hos_sa3", 
                                               "urg_ref", "urg_ref_sa1","urg_ref_sa2", "los", "comp_ref_d7", "ref_non_sev", "cured_d7", "hypox_sum", "severe_diag", 
                                               "hypo_o2_ref", "antibio_flag", "antimal_flag", "sc_fu_d7", "sc_fu_vis", "unsc_fu_d7", "unsc_fu_vis")], d0[, vars_d0_se[-1]], by="child_id", all.x=T)

write.xlsx(d0_se_ex, file.path(paste(dir_gen_dat, paste0("TIMCI_analysis_data_Senegal_", Sys.Date(), ".xlsx"), sep="/")))
write.csv(d0_se_ex, file.path(paste(dir_gen_dat, paste0("TIMCI_analysis_data_Senegal_", Sys.Date(), ".csv"), sep="/")))

