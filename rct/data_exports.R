###
# study name: TIMCI
# program name: data_exports.R 
# program purpose: program that exports datasets for modelling and merged overall dataset for analysis
# author: Silvia Cicconi  
###


##dataset for modelling
d0_mod <- d0[,c("child_id", "fid", "country", "intervention", "district", "type_der",  "category", "yg_infant", "cc","first_enc","sev_comp_d7", "sev_comp_d7_sa1", "sev_comp_d7_sa2", "sev_comp_d7_sa3", "sev_comp_d7_sa_ref1", "sev_comp_d7_sa_ref2", 
                "age_mod", "sex","cough_breath", "severe_diag","pre_care_trt", "main_cg_der", "trans_time_der", "danger_sign", "sx_lethargy", "vomit_ev_der", "eat_drink_der", "sx_difficulty_breath", "sx_less_feed", "pr_hos", "pr_hos_sa1", "pr_hos_sa2", 
                "pr_hos_sa3", "pr_hos_sa_ref1", "pr_hos_sa_ref2", "sev_comp_d28", "urg_ref", "los", "comp_ref_d7", "ref_non_sev", "cured_d7", "sev_hyp", "hypo_o2_ref", "antibio_flag", "antibio_app", "antibio_unapp", 
                "single_diag", "antibio_req", "antibio_first_line", "fev_flag", "malaria_res", "mal_flag", "antimal_flag", "antimicro_flag", "sc_fu_d7", "sc_fu_vis", "unsc_fu_d7", "unsc_fu_vis")]
save(d0_mod, file=file.path(paste(dir_gen_dat, "d0_mod.RData", sep="/")))
write.xlsx(d0_mod, file.path(paste(dir_gen_dat, "d0_mod.xlsx", sep="/")))
write.csv(d0_mod, file.path(paste(dir_gen_dat, "d0_mod.csv", sep="/")))



##select variables to be exported in the merged overall dataset
#facility characteristics, flags (cc, fu, etc.)
#primary outcomes
#secondary outcomes
d0_in_ex <- merge(d0[which(d0$country=="in"),c("child_id", "fid", "intervention", "district", "type",  "category", 
                                               "cc", "first_enc", "fu_flag_d7", "date_call_d7", "wth_flag_d7", "fu_flag_d28", "date_call_d28", "wth_flag_d28",
                                               "dth_flag", "date_death", "dth_time_flag", "dth_flag2", "date_death2", "dth_time_flag2",
                                               "hos_flag", "date_hos", "hos_time_flag", "hos_flag2", "date_hos2", "hos_time_flag2", "pr_se_hos", "pr_se_hos2",
                                               "sev_comp_d7", "sev_comp_d7_sa1", "sev_comp_d7_sa2", "sev_comp_d7_sa3", "sev_comp_d7_sa_ref2", 
                                               "pr_hos", "pr_hos_sa1", "pr_hos_sa2", "pr_hos_sa3", "pr_hos_sa_ref2", 
                                               "sev_comp_d28", "urg_ref", "urg_ref_sa2", "los", "comp_ref_d7", "ref_non_sev", "cured_d7", "hypox_sum", "severe_diag", 
                                               "hypo_o2_ref", "antibio_flag", "antimal_flag", "sc_fu_d7", "sc_fu_vis", "unsc_fu_d7", "unsc_fu_vis")], d0[, vars_d0_in[-1]], by="child_id", all.x=T)
write.xlsx(d0_in_ex, file.path(paste(dir_gen_dat, paste0("TIMCI_analysis_data_India_", Sys.Date(), ".xlsx"), sep="/")))
write.csv(d0_in_ex, file.path(paste(dir_gen_dat, paste0("TIMCI_analysis_data_India_", Sys.Date(), ".csv"), sep="/")))



d0_tz_ex <- merge(d0[which(d0$country=="tz"),c("child_id", "fid", "intervention", "district", "type",  "category", 
                 "cc", "first_enc", "fu_flag_d7", "date_call_d7", "wth_flag_d7", "fu_flag_d28", "date_call_d28", "wth_flag_d28",
                "dth_flag", "date_death", "dth_time_flag", "dth_flag2", "date_death2", "dth_time_flag2",
                "hos_flag", "date_hos", "hos_time_flag", "hos_flag2", "date_hos2", "hos_time_flag2", "pr_se_hos", "pr_se_hos2",
                "sev_comp_d7", "sev_comp_d7_sa1", "sev_comp_d7_sa2", "sev_comp_d7_sa3", "sev_comp_d7_sa_ref1", "sev_comp_d7_sa_ref2", 
                "pr_hos", "pr_hos_sa1", "pr_hos_sa2", "pr_hos_sa3", "pr_hos_sa_ref1", "pr_hos_sa_ref2", 
                "sev_comp_d28", "urg_ref", "urg_ref_sa1", "urg_ref_sa2", "los", "comp_ref_d7", "ref_non_sev", "cured_d7", "hypox_sum", "severe_diag", 
                "hypo_o2_ref", "antibio_flag", "antimal_flag", "sc_fu_d7", "sc_fu_vis", "unsc_fu_d7", "unsc_fu_vis")], d0[, vars_d0_tz[-1]], by="child_id", all.x=T)

write.xlsx(d0_tz_ex, file.path(paste(dir_gen_dat, paste0("TIMCI_analysis_data_Tanzania_", Sys.Date(), ".xlsx"), sep="/")))
write.csv(d0_tz_ex, file.path(paste(dir_gen_dat, paste0("TIMCI_analysis_data_Tanzania_", Sys.Date(), ".csv"), sep="/")))

