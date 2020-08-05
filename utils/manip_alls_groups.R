###############################################################
###### DATA WRANGLING OUTPUTS FROM COVIDM COUNTRY REPORTS #####
##############################################################

#### DATA DOWNLOADED 23/07/2020 FROM SHARED FOLDER
## SET UP: 
## must have folder "covidm_hpc_output" in your working directory
### with the individual country folders stored, with the following files in each country folder;
# 001.qs: the full simulation results (i.e. each of 500 runs) for the unmitigated epidemic, by outcome and age category
# alls.qs: the quantiled results for all scenarios
# accs.qs: the quantiled cumulative comparisons (to the unmitigated epidemic) for all scenarios
# peak.qs: the timing and value of the outcomes by scenario, also quantiled
### must have folder "outputs" to store outptuts in

require(qs)
require(data.table)
require(stringr)

list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "alls.qs$", 
                            full.names = TRUE)

### this function focuses on creating cumulative estimates 

alls_wrangle_function <- function(list.files, group1, time1){
  
  group1 <-group1 ## group 1 is the age group you want
  time1 <- time1 ## time1 is the cut-off time you want to sum over
  ## e.g. if time1=365, then the code sums over values from t=0 to t=365
  
  ### due to processing limitations do process in chunks
  break_up_calc <- function(list_of_files,nstart, nstop, measure, group0, timestop){
    
    ## list_of_files is a vector of characters listing the file names
    ## nstart is a numeric with the chunk start value
    ## nstop is a numeric with the chunk stop value
    ## measure is a character variable of "med", "lo.lo", "hi.hi","lo" or "hi".
    ## group0 is a character variable of  "all",  "<14",  "15-29", "30-44", "45-59", "60+"  
    ## timestop is the time in the model you want to then estimate the cumulative number of cases (e.g. 365 days)
    
    if(class(measure)!="character") stop('measure is not a character')
    if(class(measure)!="character") stop('group is not a character')
    
    list_of_files_T <- list_of_files[as.numeric(nstart):as.numeric(nstop)]
    # Read all the files and create a FileName column to store filenames
    DT <- rbindlist(sapply(list_of_files_T, qread, simplify = FALSE),
                    use.names = TRUE, idcol = "FileName",fill=TRUE)
    DT <- DT[age==group0 & t<=as.numeric(timestop)]  ## subset what you want
    DT[ , country := str_sub(FileName,-11,-9)] ## take country code from file name
    output <- DT[,.(year.sum=sum(get(measure))), by=list(country,compartment,scen_id)] 
    return(output)
    gc()
  }
  
  med1 <- break_up_calc(list_of_files,1,50,"med",group1, time1)
  med2 <- break_up_calc(list_of_files,51,98,"med",group1, time1)
  lolo1 <- break_up_calc(list_of_files,1,50,"lo.lo",group1, time1)
  lolo2 <- break_up_calc(list_of_files,51,98,"lo.lo",group1, time1)
  hihi1 <- break_up_calc(list_of_files,1,50,"hi.hi",group1, time1)
  hihi2 <- break_up_calc(list_of_files,51,98,"hi.hi",group1, time1)
  
  #### getting the same for different variables (median , IQR, CI)
  adapt_chunks <- function(chunk1,chunk2,suff){
    ## chunk1 is a data.table from the first chunk of processing
    ## chunk 2 is a data.table from the second chunk of processing
    ## suff is the suffix wanted to attach to this representing whether median, lolo etc.
    
    together <- rbind(chunk1,chunk2)
    together <- together[compartment!="R"] ## removes R compartment, remove line if want to keep
    together <- dcast(together, country + scen_id ~ compartment, value.var="year.sum", fill=0) 
    ## ^ reshapes to have numbers in each compartment as columns aligned with country & scenario
    colnames(together)[3:7] <- paste(colnames(together)[3:7], suff, sep = "_")
    return(together)
  }
  
  med <- adapt_chunks(med1,med2,"med")
  lolo <- adapt_chunks(lolo1,lolo2,"lolo")
  hihi <- adapt_chunks(hihi1,hihi2,"hihi")
  
  all <- merge(med, lolo, by=c("country","scen_id")) ## merge together to have one data file for all variables
  all <- merge(all, hihi, by=c("country","scen_id"))
  
  ##  reorder columns if wanted
  setcolorder(all, c("country","scen_id" , 
                     "cases_lolo","cases_med","cases_hihi",
                     "hosp_p_lolo","hosp_p_med","hosp_p_hihi",
                     "icu_p_lolo","icu_p_med","icu_p_hihi",
                     "nonicu_p_lolo","nonicu_p_med","nonicu_p_hihi",
                     "death_o_lolo","death_o_med","death_o_hihi" ))
  
  return(all)
}

#### example of how to run the functions
# output_all_365 <- alls_wrangle_function(list_of_files, "all",365)
## saving outputs
# save(output_all_365, file="outputs/all_365.RData")
# write.csv(output_all_365, file="outputs/all_365.csv")

