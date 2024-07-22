###
# study name: TIMCI
# program name: functions.R  
# program purpose: customized functions
# author: Silvia Cicconi  
###


#BASELINE CHARACTERISITCS SUMMARY

bs.sum <- function(x, by, names, med.c=T, mean.c=F, minmax.c=F, tot=F, dig=F){
  
  TAB <- NULL
  num <- as.numeric(ftable(by)) #tot participants by groups 
  num.tot <- sum(num) #tot participants total 
  
  
  
  for(i in 1:ncol(x)){
    
    
    ## Continuous Variables
    if(class(x[,i])=="integer"|class(x[,i])=="numeric"){
      
      if(dig==T){      
        dig <- signif(x[,i] - as.integer(x[,i])) #calculate difference between number and corresponding integer (=decimals)
        dig <- max(nchar(sub('^0+','',sub('\\.','',dig))), na.rm=T) #maximum number of digits after the zero
      }else{
        dig <- 0
      }
      
      n <- as.numeric(table(is.na(x[,i]), by)[1,]) #non-missing values by groups (is.na=F)
      n.tot <- as.numeric(table(is.na(x[,i]))[1]) #non-missing values total (is.na=F)
      
      med <- tapply(x[,i], by, quantile, probs=0.5, na.rm=T) #median by groups
      med.tot <- quantile(x[,i], probs=0.5, na.rm=T) #median total
      
      q1 <- tapply(x[,i], by, quantile, probs=0.25, na.rm=T) #1st quartile by groups
      q1.tot <- quantile(x[,i], probs=0.25, na.rm=T) #1st quartile total
      
      q3 <- tapply(x[,i], by, quantile, probs=0.75, na.rm=T) #3rd quartile by groups
      q3.tot <- quantile(x[,i], probs=0.75, na.rm=T) #3st quartile total
      
      mean <- tapply(x[,i], by, mean, na.rm=T) #quantile by groups
      mean.tot <- mean(x[,i], na.rm=T) #quantile total
      
      sd <- tapply(x[,i], by, sd, na.rm=T) #sd by groups
      sd.tot <- sd(x[,i], na.rm=T) #sd total
      
      min <- tapply(x[,i], by, min, na.rm=T) #min by groups
      min.tot <- min(x[,i], na.rm=T) #min total
      
      max <- tapply(x[,i], by, max, na.rm=T) #max by groups
      max.tot <- max(x[,i], na.rm=T) #max total
      
      
      #combine data for table              
      sum.tab <- NULL
      
      if(med.c==T){
        sum.tab <- rbind(sum.tab, c("", "Median (IQR)", paste(formatC(round(med, dig+1), format="f", dig+1), " (", formatC(round(q1, dig+2), format="f", dig+2), ", ", formatC(round(q3, dig+2), format="f", dig+2), ")", sep=""), paste(formatC(round(med.tot, dig+1), format="f", dig+1), " (", formatC(round(q1.tot, dig+2), format="f", dig+2), ", ", formatC(round(q3.tot, dig+2), format="f", dig+2), ")", sep=""))) #median (IQR)
      }
      if(mean.c==T){
        sum.tab <- rbind(sum.tab, c("", "Mean (SD)", paste(formatC(round(mean, dig+1), format="f", dig+1), " (", formatC(round(sd, dig+2), format="f", dig+2), ")", sep=""), paste(formatC(round(mean.tot, dig+1), format="f", dig+1), " (", formatC(round(sd.tot, dig+2), format="f", dig+2), ")", sep="") )) #mean (SD)
      }
      if(minmax.c==T){
        sum.tab <- rbind(sum.tab, c("", "Min-Max", paste(formatC(round(min, dig), format="f", dig), ", ", formatC(round(max, dig), format="f", dig), sep=""), paste(formatC(round(min.tot, dig), format="f", dig), ", ", formatC(round(max.tot, dig), format="f", dig), sep=""))) #Min-Max
      }
      
      sum.tab[1,1] <- names[i]
      
      #add missing, if present
      if(length(is.na(x[,i])[which(is.na(x[,i])==T)])>0){
        
        na <- as.numeric(table(is.na(x[,i]), by)[2,]) #missing values by groups (is.na=T)
        na.tot <- as.numeric(table(is.na(x[,i]))[2]) #missing values total (is.na=T)
        
        sum.tab <- rbind(sum.tab, c("", "Unknown", paste(formatC(round(na/num*100, 1), format="f", 1), "% (", na, ")", sep=""), paste(formatC(round(na.tot/num.tot*100, 1), format="f", 1),  "% (", na.tot, ")", sep=""))) #missing number
      }
      
      TAB <- rbind(TAB, sum.tab)
      
    }
    
    
    
    ## Factor variables
    if(class(x[,i])!="integer" & class(x[,i])!="numeric"){
      
      
      n <- as.numeric(table(is.na(x[,i]), by)[1,]) #non-missing values by groups (is.na=F)
      n.tot <- as.numeric(table(is.na(x[,i]))[1]) #non-missing values total (is.na=F)
      
      fr <- table(x[,i], by) #frequency by groups
      fr.tot <- table(x[,i]) #frequency total
      
      #combine data for table   
      sum.tab <- NULL
      
      for(j in 1:nrow(fr)){
        
        sum.tab <- rbind(sum.tab, c("", rownames(fr)[j], paste(formatC(round(fr[j,]/num*100, 1), format="f", 1), "% (", fr[j,], ")", sep=""),   paste(formatC(round(fr.tot[j]/num.tot*100, 1), format="f", 1), "% (", fr.tot[j], ")", sep="")))
        
      }
      sum.tab[1,1] <- names[i]
      
      #add missing, if present
      if(length(is.na(x[,i])[which(is.na(x[,i])==T)])>0){
        
        na <- as.numeric(table(is.na(x[,i]), by)[2,]) #missing values by groups (is.na=T)
        na.tot <- as.numeric(table(is.na(x[,i]))[2]) #missing values total (is.na=T)
        
        sum.tab <- rbind(sum.tab, c("", "Unknown", paste(formatC(round(na/num*100, 1), format="f", 1), "% (", na, ")", sep=""), paste(formatC(round(na.tot/num.tot*100, 1), format="f", 1), "% (", na.tot, ")", sep=""))) #missing number
      }
      
      TAB <- rbind(TAB, sum.tab)  
      
    }
    
  }    
  
  #format
  TAB <- as.data.frame(TAB)
  
  
  #replace any NaN with "-" (for percentages calculated on denominator =0)
  TAB[TAB=="0 (NaN%)"] <- "-"
  
  #col names
  names(TAB) <- c("Variables", "Statistics/Categories", paste(levels(as_label(by)), " (n=", num, ")", sep=""), paste("Overall (n=", num.tot, ")")) 
  
  #remove overall column if not required
  if(tot==F){
    TAB <- TAB[, 1:(ncol(TAB)-1)]
  }
  
  row.names(TAB) <- NULL
  TAB
  
}



#ONE ROW SUMMARY

onerow.sum.tab <- function(x, vars, vars_name, by, med.c=T, mean.c=F, ci=F, dig=F){
  
  tab <- NULL
  
  for(k in 1:length(vars)){
    
    ## Continuous variables
    if(class(x[,vars[k]])=="integer" | class(x[,vars[k]])=="numeric"){
      
      if(dig==T){      
        dig <- signif(x[,i] - as.integer(x[,i])) #calculate difference between number and corresponding integer (=decimals)
        dig <- max(nchar(sub('^0+','',sub('\\.','',dig))), na.rm=T) #maximum number of digits after the zero
      }else{
        dig <- 0
      }
      
      
      n.tot <- as.numeric(table(is.na(x[,vars[k]]), x[, by])[1,]) #non-missing values (is.na=F)
      for(i in country_list){
        assign(paste("n", `i`, sep="."), table(is.na(x[which(x$country==`i`), vars[k]]), x[which(x$country==`i`), by])[1,]) #n by country
      }
      
      med.tot <- tapply(x[,vars[k]], x[,by], quantile, probs=0.5, na.rm=T) #median
      for(i in country_list){
        assign(paste("med", `i`, sep="_"), tapply(x[which(x$country==`i`),vars[k]], x[which(x$country==`i`),by], quantile, probs=0.5, na.rm=T)) #median by country
      }
      
      q1.tot <- tapply(x[,vars[k]], x[,by], quantile, probs=0.25, na.rm=T) #1st quartile
      for(i in country_list){
        assign(paste("q1", `i`, sep="_"), tapply(x[which(x$country==`i`),vars[k]], x[which(x$country==`i`),by], quantile, probs=0.25, na.rm=T)) #1st quartile by country
      }
      
      q3.tot <- tapply(x[,vars[k]], x[,by], quantile, probs=0.75, na.rm=T) #3rd quartile
      for(i in country_list){
        assign(paste("q3", `i`, sep="_"), tapply(x[which(x$country==`i`),vars[k]], x[which(x$country==`i`),by], quantile, probs=0.75, na.rm=T)) #3rd quartile by country
      }
      
      mean.tot <- tapply(x[,vars[k]], x[,by], mean, na.rm=T) #mean quartile
      for(i in country_list){
        assign(paste("mean", `i`, sep="_"), tapply(x[which(x$country==`i`),vars[k]], x[which(x$country==`i`),by], mean, na.rm=T)) #mean by country
      }
      
      sd.tot <- tapply(x[,vars[k]], x[,by], sd, na.rm=T) #sd quartile
      for(i in country_list){
        assign(paste("sd", `i`, sep="_"), tapply(x[which(x$country==`i`),vars[k]], x[which(x$country==`i`),by], sd, na.rm=T)) #sd by country
      }
      
      #combine data for table              
      if(nlevels(d0[,by])==2)sum.tab <- c("p1"="","p2"="")
      if(nlevels(d0[,by])==3)sum.tab <- c("p1"="","p2"="","p3"="")
      
      if(med.c==T){
        sum.tab <- rbind(sum.tab, rbind(c(paste(formatC(round(med.tot[1:2], dig+1), format="f", dig+1), " (", formatC(round(q1.tot[1:2], dig+2), format="f", dig+2), ", ", formatC(round(q3.tot[1:2], dig+2), format="f", dig+2), "), ", n.tot[1:2], sep=""), "-"), c(paste(formatC(round(med_in[1:2], dig+1), format="f", dig+1), " (", formatC(round(q1_in[1:2], dig+2), format="f", dig+2), ", ", formatC(round(q3_in[1:2], dig+2), format="f", dig+2), "), ", n.in[1:2], sep=""), "-"), paste(formatC(round(med_tz, dig+1), format="f", dig+1), " (", formatC(round(q1_tz, dig+2), format="f", dig+2), ", ", formatC(round(q3_tz, dig+2), format="f", dig+2), "), ", n.tz, sep=""))) #median (IQR)
      }
      if(mean.c==T){
        sum.tab <- rbind(sum.tab, rbind(c(paste(formatC(round(mean.tot[1:2], dig+1), format="f", dig+1), " (", formatC(round(sd.tot[1:2], dig+2), format="f", dig+2), "), ", n.tot[1:2], sep=""), "-"), c(paste(formatC(round(mean_in[1:2], dig+1), format="f", dig+1), " (", formatC(round(sd_in[1:2], dig+2), format="f", dig+2), "), ", n.in[1:2], sep=""),"-"),  paste(formatC(round(mean_tz, dig+1), format="f", dig+1), " (", formatC(round(sd_tz, dig+2), format="f", dig+2), "), ", n.tz, sep=""))) #mean (SD)
      }
      
      #add row names
      sum.tab <- as.data.frame(cbind(c(vars_name[k], "Combined", "India", "Tanzania"), sum.tab))
      names(sum.tab) <- c("", names(sum.tab)[2:length(names(sum.tab))])
      
      tab <- rbind(tab, sum.tab)
    }
    
    
    ## Factor variables
    if(class(x[,vars[k]])!="integer" & class(x[,vars[k]])!="numeric"){
      #frequencies
      fr <- table(x$country[which(x[,vars[k]]==1)], x[which(x[,vars[k]]==1), by])
      fr <- rbind(margin.table(fr, 2), fr)
      
      #totals
      num <- table(x$country, x[, by])
      num <- rbind(margin.table(num, 2), num)
      
      #combine data for table 
      if(nlevels(d0[,by])==2) sum.tab <- data.frame("p1"="", "p2"="")
      if(nlevels(d0[,by])==3) sum.tab <- data.frame("p1"="","p2"="", "p3"="")
      
      for(j in 1:nrow(fr)){
        #calculate percentages
        perc <- paste(formatC(round(fr[j,]/num[j,]*100, 1), format="f", 1), "%", sep="")
        
        #combine frequency and totals
        fr_n <- paste("(", fr[j,], "/", num[j,], ")", sep="")
        
        if(ci==T){
          #calculate confidence intervals
          for(i in 1:ncol(fr)){
            bin <- binom.test(fr[j,i], num[j,i])
            ci <- paste("(", formatC(round(bin$conf.int[1]*100, 1), format="f", 1), "%, ", formatC(round(bin$conf.int[2]*100, 1), format="f", 1), "%)", sep="")
          }
        }
        
        #combine all summaries
        if(ci==T){
          sum.tab[j+1,] <- paste(perc, ci, fr_n) 
        }else{
          sum.tab[j+1,] <- paste(perc, fr_n) 
        }
      }
      
      #add row names
      sum.tab <- cbind(c(vars_name[k], "Combined", "India", "Tanzania"), sum.tab)
      names(sum.tab) <- c("", names(sum.tab)[2:length(names(sum.tab))])
      
      tab <- rbind(tab, sum.tab)
      tab}
  }
  
  #format
  tab <- as.data.frame(tab)  
  
  #replace NaN% with "-"
  tab[tab=="NaN% (0/0)"] <- "- (0/0)"
  
  
  
  #add col names
  if(by=="intervention"){
    names(tab) <- c("", "Control", "PO", "PO+CDSA")
    #replace with "-" CDSA for India and combined
    tab[which(tab[,1]%in%c("India","Combined")), ncol(tab)] <- "-"
  }
  tab
  
}


#GENERAL SUMMARY TABLE RESULTS

sum.tab <- function(x, var, by, ci=F){
  
  ###young infants
  #frequencies
  fr_yi <- table(x$country[which(x[,var]==1 & x$yg_infant==1)], x[which(x[,var]==1 & x$yg_infant==1), by])
  fr_yi <- rbind(margin.table(fr_yi, 2), fr_yi)
  
  #totals
  num_yi <- table(x$country[which(x$yg_infant==1)], x[which(x$yg_infant==1), by])
  num_yi <- rbind(margin.table(num_yi, 2), num_yi)
  
  ###older children
  #frequencies
  fr_ch <- table(x$country[which(x[,var]==1 & x$yg_infant==0)], x[which(x[,var]==1 & x$yg_infant==0), by])
  fr_ch <- rbind(margin.table(fr_ch, 2), fr_ch)
  
  #totals
  num_ch <- table(x$country[which(x$yg_infant==0)], x[which(x$yg_infant==0), by])
  num_ch <- rbind(margin.table(num_ch, 2), num_ch)
  
  #combine data for table 
  sum.tab_yi <- data.frame("Control"="", "PO"="", "PO+CDSA"="")
  sum.tab_ch <- data.frame("Control"="", "PO"="", "PO+CDSA"="")
  for(j in 1:nrow(fr_yi)){
    #calculate percentages
    perc_yi <- paste(formatC(round(fr_yi[j,]/num_yi[j,]*100, 1), format="f", 1), "%", sep="")
    perc_ch <- paste(formatC(round(fr_ch[j,]/num_ch[j,]*100, 1), format="f", 1), "%", sep="")
    
    #combine frequency and totals
    fr_n_yi <- paste("(", fr_yi[j,], "/", num_yi[j,], ")", sep="")
    fr_n_ch <- paste("(", fr_ch[j,], "/", num_ch[j,], ")", sep="")
    
    if(ci==T){
      #calculate confidence intervals
      for(i in 1:ncol(fr_yi)){
        bin_yi <- binom.test(fr_yi[j,i], num_yi[j,i])
        ci_yi <- paste("[", formatC(round(bin_yi$conf.int[1]*100, 1), format="f", 1), "%, ", formatC(round(bin_yi$conf.int[2]*100, 1), format="f", 1), "%]", sep="")
        bin_ch <- binom.test(fr_ch[j,i], num_ch[j,i])
        ci_ch <- paste("[", formatC(round(bin_ch$conf.int[1]*100, 1), format="f", 1), "%, ", formatC(round(bin_ch$conf.int[2]*100, 1), format="f", 1), "%]", sep="")
      }
    }
    
    
    #combine all summaries
    if(ci==T){
      sum.tab_yi[j+1,] <- paste(perc_yi, ci_yi, fr_n_yi) 
      sum.tab_ch[j+1,] <- paste(perc_ch, ci_ch, fr_n_ch)  
    }else{
      sum.tab_yi[j+1,] <- paste(perc_yi, fr_n_yi) 
      sum.tab_ch[j+1,] <- paste(perc_ch, fr_n_ch)
    }
    
  }
  
  ##Combine summaries together
  sum.tab <- rbind(sum.tab_yi, sum.tab_ch)
  
  #add row names
  sum.tab <- cbind(c("Under 2 months of age", "Combined", "India", "Tanzania", "2-59 months of age", "Combined", "India", "Tanzania"), sum.tab)
  
  #replace NaN% with "-"
  sum.tab[sum.tab=="NaN% (0/0)"] <- "- (0/0)"
  
  #replace with "-" CDSA for India and combined
  sum.tab[which(sum.tab[,1]%in%c("India","Combined")), ncol(sum.tab)] <- "-"
  
  #add col names
  names(sum.tab) <- c("", "Control", "PO", "PO+CDSA")
  sum.tab
  
}


#GENERAL SUMMARY TABLE RESULTS WITH CAT SUMMARY

sum.tab.cat <- function(x, var, var_name, var2, by){
  
  ##without restriction
  if(is.null(var2)){
    ###young infants
    #frequencies
    fr_yi_co <- table(x[which(x$yg_infant==1),var], x[which(x$yg_infant==1),  by])
    for(i in country_list){assign(paste("fr_yi", `i`, sep="_"), table(x[which(x$yg_infant==1 & x$country==`i`), var], x[which(x$yg_infant==1  & x$country==`i`),  by]))}
    
    #totals (adapt to have the totals repeated for each row)
    num_yi_co <- table(x[which(x$yg_infant==1),  by])
    tmp <- fr_yi_co; for(k in 1:nrow(tmp)){tmp[k,] <- num_yi_co}; num_yi_co <- tmp
    for(i in country_list){
      assign(paste("num_yi", `i`, sep="_"), table(x[which(x$yg_infant==1  & x$country==`i`), by]))
      tmp <- get(paste("fr_yi", `i`, sep="_"))
      for(k in 1:nrow(tmp)){
        tmp[k,] <- get(paste("num_yi", `i`, sep="_"))
      }
      assign(paste("num_yi", `i`, sep="_"), tmp)
    }
    
    ###older children
    #frequencies
    fr_ch_co <- table(x[which(x$yg_infant==0), var], x[which(x$yg_infant==0),  by])
    for(i in country_list){assign(paste("fr_ch", `i`, sep="_"), table(x[which(x$yg_infant==0 & x$country==`i`), var], x[which(x$yg_infant==0  & x$country==`i`), by]))}
    
    #totals (adapt to have the totals repeated for each row)
    num_ch_co <- table(x[which(x$yg_infant==0),  by])
    tmp <- fr_ch_co; for(k in 1:nrow(tmp)){tmp[k,] <- num_ch_co}; num_ch_co <- tmp
    for(i in country_list){
      assign(paste("num_ch", `i`, sep="_"), table(x[which(x$yg_infant==0  & x$country==`i`), by]))
      tmp <- get(paste("fr_ch", `i`, sep="_"))
      for(k in 1:nrow(tmp)){
        tmp[k,] <- get(paste("num_ch", `i`, sep="_"))
      }
      assign(paste("num_ch", `i`, sep="_"), tmp)
    }
    
  }
  
  
  ##with restriction
  if(!is.null(var2)){
    ###young infants
    #frequencies
    fr_yi_co <- table(x[which(x$yg_infant==1 & x[,var2]==1), var], x[which(x$yg_infant==1  & x[,var2]==1),  by])
    for(i in country_list){assign(paste("fr_yi", `i`, sep="_"), table(x[which(x$yg_infant==1 & x$country==`i`  & x[,var2]==1), var], x[which(x$yg_infant==1  & x$country==`i` & x[,var2]==1),  by]))}
    
    #totals
    num_yi_co <- table(x[which(x$yg_infant==1), var], x[which(x$yg_infant==1),  by])
    for(i in country_list){assign(paste("num_yi", `i`, sep="_"), table(x[which(x$yg_infant==1  & x$country==`i`), var], x[which(x$yg_infant==1  & x$country==`i`), by]))}
    
    ###older children
    #frequencies
    fr_ch_co <- table(x[which(x$yg_infant==0 & x[,var2]==1), var], x[which(x$yg_infant==0 & x[,var2]==1),  by])
    for(i in country_list){assign(paste("fr_ch", `i`, sep="_"), table(x[which(x$yg_infant==0 & x$country==`i` & x[,var2]==1), var], x[which(x$yg_infant==0  & x$country==`i` & x[,var2]==1), by]))}
    
    #totals
    num_ch_co <- table(x[which(x$yg_infant==0), var], x[which(x$yg_infant==0), by])
    for(i in country_list){assign(paste("num_ch", `i`, sep="_"), table(x[which(x$yg_infant==0  & x$country==`i`), var], x[which(x$yg_infant==0  & x$country==`i`), by]))}
  }
  
  
  #combine data for table 
  sum.tab <- NULL
  for(j in 1:ncol(fr_yi_co)){
    #calculate percentages
    perc_yi_co <- paste(formatC(round(fr_yi_co[,j]/num_yi_co[,j]*100, 1), format="f", 1), "%", sep="")
    perc_ch_co <-  paste(formatC(round(fr_ch_co[,j]/num_ch_co[,j]*100, 1), format="f", 1), "%", sep="")
    for(i in country_list){
      assign(paste("perc_yi", `i`, sep="_"), paste(formatC(round(get(paste("fr_yi", `i`, sep="_"))[,j]/get(paste("num_yi", `i`, sep="_"))[,j]*100, 1), format="f", 1), "%", sep=""))
      assign(paste("perc_ch", `i`, sep="_"), paste(formatC(round(get(paste("fr_ch", `i`, sep="_"))[,j]/get(paste("num_ch", `i`, sep="_"))[,j]*100, 1), format="f", 1), "%", sep=""))
    }
    
    #combine frequency and totals
    fr_n_yi_co <- paste("(", fr_yi_co[,j], "/", num_yi_co[,j], ")", sep="")
    fr_n_ch_co <- paste("(", fr_ch_co[,j], "/", num_ch_co[,j], ")", sep="")
    for(i in country_list){
      assign(paste("fr_n_yi", `i`, sep="_"), paste("(", get(paste("fr_yi", `i`, sep="_"))[,j], "/", get(paste("num_yi", `i`, sep="_"))[,j], ")", sep=""))
      assign(paste("fr_n_ch", `i`, sep="_"), paste("(", get(paste("fr_ch", `i`, sep="_"))[,j], "/", get(paste("num_ch", `i`, sep="_"))[,j], ")", sep=""))
    }
    
    #combine all summaries
    temp <- c("", paste(perc_yi_co, fr_n_yi_co), paste(perc_yi_in, fr_n_yi_in), paste(perc_yi_tz, fr_n_yi_tz), "", paste(perc_ch_co, fr_n_ch_co), paste(perc_ch_in, fr_n_ch_in), paste(perc_ch_tz, fr_n_ch_tz))
    
    sum.tab <- cbind(sum.tab, temp)
  }
  
  #add row names
  sum.tab <- cbind(c("Under 2 months of age", "Combined", rep("", (nlevels(x[,var])-1)), "India", rep("", (nlevels(x[,var])-1)), "Tanzania", rep("", (nlevels(x[,var])-1)), "2-59 months of age", "Combined", rep("", (nlevels(x[,var])-1)), "India", rep("", (nlevels(x[,var])-1)), "Tanzania", rep("", (nlevels(x[,var])-1))), c(rep(c("", rep(levels(as_label(x[,var])),3)),2)), sum.tab)
  
  sum.tab <- as.data.frame(sum.tab)
  
  #replace NaN% with "-"
  sum.tab[sum.tab=="NaN% (0/0)"] <- "- (0/0)"
  
  #replace with "-" CDSA for India and combined
  sum.tab[which(sum.tab[,1]%in%c("India","Combined")), ncol(sum.tab)] <- "-"
  for(k in 2:nlevels(x[,var])){
    sum.tab[which(sum.tab[,1]%in%c("India","Combined"))+k-1, ncol(sum.tab)] <- "-"
  }
  
  
  #add col names
  names(sum.tab) <- c("", var_name, "Control", "PO", "PO+CDSA")
  sum.tab
  
}

#GENERATE BAR PLOTS PERCENTAGES

bar_graph_perc <- function(x, spo2_var, fid_list,  fid_list_names, pseudo.dt){
  
  #calculate percentages for graph
  tab <- prop.table(table(x[which(x$pulse_ox_ind==1), spo2_var], x$week[which(x$pulse_ox_ind==1)]), 2)[2,]*100
  
  
  ##format and reshape in a format required for the graph (multiple rows per visit, one each category)  
  dt <- data.frame("Percentage"=tab, "Time"=names(tab)) #dataframe
  
  #format time
  dt$Time <- factor(dt$Time, levels=names(tab))
  
  #plot
  gr <- ggplot(dt, aes(y=Percentage, x=Time)) + geom_bar( stat="identity", fill="lightblue") +
    scale_x_discrete(name="Weeks", labels=pseudo.dt) +  #customize x axis
    theme_bw() + #set white background 
    theme(text = element_text(size=11, face="italic"), axis.text.x = element_text(size=8, face="plain", angle = 45, hjust=1), axis.text.y = element_text(size=11, face="plain"), axis.title.x = element_text(size=13, face="plain"), axis.title.y = element_text(size=13, face="plain")) + #set which grid lines to display
    labs(title=fid_list_names) #add graph title  
  
  gr
  
}


#GENERATE BAR PLOTS TWO SIDED BARS

bar_graph_2b <- function(x, x1, fid_list, fid_list_names, pseudo.dt){
  
  #children recruited per week
  x$week <- factor(x$week, levels=seq(1,length(pseudo.dt),1))
  tab_d0 <- table(x$week)
  
  #cdsa records per week
  x1$week <- factor(x1$week, levels=seq(1,length(pseudo.dt),1))
  tab_cdsa <- table(x1$week)
  
  ##flag two sources and combine
  tab_d0 <- as.data.frame(cbind("week"=names(tab_d0), "sum"=as.numeric(tab_d0), "source"="Children enrolled"))
  tab_cdsa <- as.data.frame(cbind("week"=names(tab_cdsa), "sum"=as.numeric(tab_cdsa), "source"="Children recorded in CDSA"))
  
  dt <- rbind(tab_d0, tab_cdsa)
  
  #plot
  gr <- ggplot(dt, aes(x=week, y=as.numeric(sum), fill=factor(source))) + geom_bar(stat="identity", position="dodge", width=0.6) +
    scale_x_discrete(name="Weeks", labels=pseudo.dt) +  #customize x axis
    scale_y_continuous(name="N") +
    theme_bw() + #set white background 
    theme(text = element_text(size=11, face="italic"), axis.text.x = element_text(size=8, face="plain", angle = 45, hjust=1), axis.text.y = element_text(size=11, face="plain"), axis.title.x = element_text(size=13, face="plain"), axis.title.y = element_text(size=13, face="plain", angle=0, vjust=0.5)) + 
    labs(title=fid_list_names) + #add graph title 
    theme(legend.position = "none") #no legend
  
  gr
  
}


#GENERATE BAR PLOTS THREE SIDED BARS

bar_graph_3b <- function(x, var, lab, title){
  
  #calculate percentages for graph
  tab_yg <- prop.table(table(x[which(x$yg_infant==1), var]))*100
  tab_ch <- prop.table(table(x[which(x$yg_infant==0), var]))*100
  
  ##flag two sources and combine
  dt_yg <- data.frame("percentage"=as.numeric(tab_yg), "hyp"=lab, "pop"="Under 2 months of age") #dataframe
  dt_ch <- data.frame("percentage"=as.numeric(tab_ch), "hyp"=lab, "pop"="2-59 months of age") #dataframe
  
  dt <- rbind(dt_yg, dt_ch)
  
  #plot
  gr <- ggplot(dt, aes(x=hyp, y=as.numeric(percentage), fill=factor(pop, levels=c("Under 2 months of age", "2-59 months of age")) )) + geom_bar(stat="identity", position="dodge", width=0.6) +
    scale_x_discrete(name="SpO2") +  #customize x axis
    scale_y_continuous(name="% children") +
    theme_bw() + #set white background 
    theme(text = element_text(size=11, face="italic"), axis.text.x = element_text(size=8, face="plain", angle = 45, hjust=1), axis.text.y = element_text(size=11, face="plain"), axis.title.x = element_text(size=13, face="plain"), axis.title.y = element_text(size=13, face="plain", vjust=0.5)) + 
    labs(title=title) + #add graph title 
    theme(legend.position = "none") #no legend
  gr
  
}


#GET LEGEND

get_legend<-function(myplot){
  tmp <- ggplot_gtable(ggplot_build(myplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


#GEE MODEL SUMMARY

summMod <- function(mod){
  
  co.tab <- as.data.frame(summary(mod)$coef)
  co.tab <- co.tab[row.names(co.tab)%in%c("factor(intervention)1", "factor(intervention)2"),]
  
  #OR and 95% CI (remove intercept and keep only intervention)
  or <- exp(co.tab[,"Estimate"])
  or.up <- exp(co.tab[,"Estimate"]+1.96*co.tab[,"Robust S.E."]) 
  or.lo <- exp(co.tab[,"Estimate"]-1.96*co.tab[,"Robust S.E."])
  
  #RD and 95% CI
  require(emmeans)
  mar <- emmeans(mod, specs = "intervention", regrid = "response")
  rd.es <- as.data.frame(pairs(mar, infer = c(T,F), reverse=T))
  if(nrow(rd.es)==3){rd.es <- rd.es[1:2,]} #don't want estimate for CDSA-PO
  rd <- as.numeric(rd.es[,"estimate"])
  rd.up <- as.numeric(rd.es[,"asymp.UCL"])
  rd.lo <- as.numeric(rd.es[,"asymp.LCL"])
  
  #name and level name
  c.nam <- rownames(co.tab[,])
  c.lev <- gsub("[^0-9]", "", c.nam); c.lev[which(nchar(c.lev)>0 & substr(c.lev,1,1)==0)] <- substr(c.lev[which(nchar(c.lev)>0 & substr(c.lev,1,1)==0)],2,length(c.lev[which(nchar(c.lev)>0 & substr(c.lev,1,1)==0)])) #keep level values
  
  #p-val (from robust z test)
  p <- formatC(round((2 * (1 - pnorm(abs(co.tab[,"Robust z"])))),3), format="f", 3)
  zero.id <- which(p=="0.000")
  if(length(zero.id)>0) p[zero.id] <- "<0.001"
  
  #combine OR and RD estimates  
  est <- paste0(formatC(round(or,3), format="f", 3)," (",formatC(round(or.lo,3), format="f", 3),", ",formatC(round(or.up,3), format="f", 3),") ",
                formatC(round(rd*100,1), format="f", 1),"% (",formatC(round(rd.lo*100,1), format="f", 1),"%, ",formatC(round(rd.up*100,1), format="f", 1),"%)")
  
  
  #combine
  res <- data.frame(cbind(c.nam,c.lev,est,p))
  
  #replace NE to non-estimable or infinite estimates
  res$est[which(grepl("Inf",res$est)==T | nchar(res$est)>80)] <- "NE"
  res$p[which(res$est=="NE")] <- "-"
  
  #replace names
  names(res) <- c("Covariate","Level","OR (95% CI) RD (95%)","P-value")
  rownames(res) <- NULL
  
  res 
}



#QUICK SUMMARY FOR INCLUDING INTO MODELS TABLES

mod_sum <- function(dat){
  
  fr <- table(dat[, 2], dat[,1])
  perc <- formatC(round(prop.table(fr,2)*100, 1), format="f", 1)

    comb <- paste0(fr[2,], " (", perc[2,], "%)")
  comb
}










