###
# study name: TIMCI
# program name: recruitment.Rmd  
# program purpose: Summary of milestones, recruitment and recruitment graph
# author: Silvia Cicconi  
###


## Generate log file
rmarkdown::render(input = file.path(getwd(),"programs/master.Rmd"),
                  output_format = c("word_document"),
                  output_file = paste("TIMCI_RCT_Final_Statistical_Analysis_Report_v2.0_", Sys.Date(), "_log.docx", sep=""),
                  output_dir = file.path(getwd(), "logfiles/"),
                  params = list(echo=T, include=T, results="markup", warning=T, error=T, message=T, log=T), 
                  encoding = "UTF-8")


## Generate report
rmarkdown::render(input = file.path(getwd(),"programs/master.Rmd"),
                  output_format = c("word_document"),
                  output_file = paste("TIMCI_RCT_Final_Statistical_Analysis_Report_v2.0_", Sys.Date(), ".docx", sep=""),
                  output_dir = file.path(getwd(), "outputs/"),
                  params = list(echo=F, include=T, results=F, warning=F, error=F, message=F, log=F), 
                  encoding = "UTF-8")

