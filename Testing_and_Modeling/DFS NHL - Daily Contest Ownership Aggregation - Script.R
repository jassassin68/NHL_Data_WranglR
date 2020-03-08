#Daily Fantasy NHL - Data Agg - Daily Faceoff Lines Version

#NEXT TO DO - CONTEST RESULTS AGG, DKSALARIES AGG, DAILY DATA, PLAYER AGGS, OUTPUT

#Draw Required Libraries
library(data.table)
library(rvest)
library(XML)
library(xml2)
library(dplyr)
library(zoo)
library(stringdist)
library(rapportools)

#SET WORKING DIRECTORY (I.E. FOLDER FOR CURRENT FILE PATH)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#~~~~~~~~COMBINE YESTERDAY'S CONTEST RESULTS FILES TO AGGREGATE FILE~~~~~~~~~~~~

#********* CASH GAME CONTEST RESULTS AGGREGATION ***********


#READ IN AGGREGATE DATA FILES
#Cash Game File
player_perc_drafted_agg <- read.csv(file = "C:/Users/jasselin/DataCamp Projects - Jupyter Notebooks/NHLData/2 - Data/Contest Ownership/player_perc_drafted_cash.csv")
#GPP Game File
player_perc_drafted_gpp_agg <- read.csv(file = "C:/Users/jasselin/DataCamp Projects - Jupyter Notebooks/NHLData/2 - Data/Contest Ownership/player_perc_drafted_gpp.csv")



#LOOP THRU CONTEST RESULTS DATES MISSING FROM AGG FILE
max_cash_results_date <- max(as.Date(player_perc_drafted_agg$Date))
max_gpp_results_date <- max(as.Date(player_perc_drafted_gpp_agg$Date))


contest_results_cash_date <- max_cash_results_date + 1

while (contest_results_cash_date < today) {
  
  #Create day, month, year variables
  contest_res_cash_day <- substr(contest_results_cash_date, 9, 10)
  contest_res_cash_month <- substr(contest_results_cash_date, 6, 7)
  contest_res_cash_year <- substr(contest_results_cash_date, 1, 4)
  
  
  
  tryCatch( {
    
    # READ IN FILE
    
    nhl_results <- read.csv(file = paste0("C:/Users/jasselin/DataCamp Projects - Jupyter Notebooks/NHLData/2 - Data/Contest Ownership/Double Up - ",
                                          toString(contest_res_cash_month),
                                          toString(contest_res_cash_day),
                                          toString(substr(contest_res_cash_year, 3, 4)),
                                          ".csv"
    ), stringsAsFactors = FALSE
    )
    
    #CLEAN UP FILE
    nhl_results <- nhl_results %>% select(1,2,3,5,6,8,10,11)
    
    #RENAME COLUMNS
    names(nhl_results) <- c("Rank",
                            "EntryID",
                            "UserName",
                            "TotalDKPts",
                            "Lineup",
                            "Name",
                            "PercDrafted",
                            "DKPts")
    
    #SUBSET TO SELECT ONLY FOR PLAYER DRAFT %
    player_perc_drafted <- nhl_results %>% select("Name", "PercDrafted", "DKPts")
    
    #Add Date column
    player_perc_drafted["Date"] <- contest_results_cash_date
    
    #ADD DATA TO AGG FILE
    player_perc_drafted_agg <- rbind(player_perc_drafted, player_perc_drafted_agg)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #INCREMENT DAY BY 1
  contest_results_cash_date <- contest_results_cash_date + 1
  
}

#DELETE FROM AGG FILE WHERE PercDrafted is null
player_perc_drafted_agg <- player_perc_drafted_agg[!is.na(player_perc_drafted_agg$PercDrafted),]

player_perc_drafted_agg <- player_perc_drafted_agg[!player_perc_drafted_agg$PercDrafted == "",]


#WRITE TO DROPBOX
write.csv(player_perc_drafted_agg,
          file = "C:/Users/jasselin/DataCamp Projects - Jupyter Notebooks/NHLData/2 - Data/Contest Ownership/player_perc_drafted_cash.csv",
          row.names = FALSE
)



#********* GPP CONTEST RESULTS AGGREGATION ***********

contest_results_gpp_date <- max_gpp_results_date + 1

while (contest_results_gpp_date < today) {
  
  #Create day, month, year variables
  contest_res_gpp_day <- substr(contest_results_gpp_date, 9, 10)
  contest_res_gpp_month <- substr(contest_results_gpp_date, 6, 7)
  contest_res_gpp_year <- substr(contest_results_gpp_date, 1, 4)
  
  
  
  tryCatch( {
    
    # READ IN FILE
    
    nhl_results <- read.csv(file = paste0("../Historical Data and Projections/Contest Results/GPP/GPP - ",
                                          toString(contest_res_gpp_month),
                                          toString(contest_res_gpp_day),
                                          toString(substr(contest_res_gpp_year, 3, 4)),
                                          ".csv"
    ), stringsAsFactors = FALSE
    )
    
    #CLEAN UP FILE
    nhl_results <- nhl_results %>% select(1,2,3,5,6,8,10,11)
    
    #RENAME COLUMNS
    names(nhl_results) <- c("Rank",
                            "EntryID",
                            "UserName",
                            "TotalDKPts",
                            "Lineup",
                            "Name",
                            "PercDrafted",
                            "DKPts")
    
    #SUBSET TO SELECT ONLY FOR PLAYER DRAFT %
    player_perc_drafted <- nhl_results %>% select("Name", "PercDrafted", "DKPts")
    
    #Add Date column
    player_perc_drafted["Date"] <- contest_results_gpp_date
    
    #ADD DATA TO AGG FILE
    player_perc_drafted_gpp_agg <- rbind(player_perc_drafted, player_perc_drafted_gpp_agg)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #INCREMENT DAY BY 1
  contest_results_gpp_date <- contest_results_gpp_date + 1
  
}

#DELETE FROM AGG FILE WHERE PercDrafted is null
player_perc_drafted_gpp_agg <- player_perc_drafted_gpp_agg[!is.na(player_perc_drafted_gpp_agg$PercDrafted),]

player_perc_drafted_gpp_agg <- player_perc_drafted_gpp_agg[!player_perc_drafted_gpp_agg$PercDrafted == "",]


#WRITE TO DROPBOX
write.csv(player_perc_drafted_gpp_agg,
          file = "C:/Users/jasselin/DataCamp Projects - Jupyter Notebooks/NHLData/2 - Data/Contest Ownership/player_perc_drafted_gpp.csv",
          row.names = FALSE
)


