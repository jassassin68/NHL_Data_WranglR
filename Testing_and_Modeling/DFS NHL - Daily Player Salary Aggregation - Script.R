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


#~~~~~~~~COMBINE YESTERDAY'S DKSalaries FILES TO AGGREGATE FILE~~~~~~~~~~~~
dk_salaries_final <- read.csv(file = "C:/Users/jasselin/DataCamp Projects - Jupyter Notebooks/NHLData/2 - Data/Contest Player Salaries/dk_salaries_final.csv")


#LOOP THRU CONTEST RESULTS DATES MISSING FROM AGG FILE
max_dk_salaries_date <- max(as.Date(dk_salaries_final$Date))

dk_salaries_date <- max_dk_salaries_date + 1



while (dk_salaries_date < Sys.Date()) {
  
  #TO HANDLE ERRORS
  
  tryCatch( {
    
    
    
    #CREATE DATE VARIABLES
    dk_salaries_day <- substr(dk_salaries_date, 9, 10)
    dk_salaries_month <- substr(dk_salaries_date, 6, 7)
    dk_salaries_year <- substr(dk_salaries_date, 1, 4)
    
    # READ IN FILE
    
    dksalaries <- read.csv(file = paste0("C:/Users/jasselin/DataCamp Projects - Jupyter Notebooks/NHLData/2 - Data/Contest Player Salaries/DKSalaries-",
                                         toString(dk_salaries_month),
                                         toString(dk_salaries_day),
                                         toString(substr(dk_salaries_year, 3, 4)),
                                         ".csv"
    ), stringsAsFactors = FALSE
    )
    
    #Find distinct teams
    dk_distinct_teams <- dksalaries %>% distinct(TeamAbbrev)
    
    #Assign distinct number of games to variable
    dk_games <- nrow(dk_distinct_teams) / 2
    
    #CLEAN UP FILE
    dksalaries <- dksalaries %>% select(1,3,5,6,9)
    
    #Add Date column
    dksalaries["Date"] <- dk_salaries_date
    
    #Rename and reorder columns
    names(dksalaries) <- c("NHL_Position",
                           "Name",
                           "DK_Position",
                           "Salary",
                           "AvgPointsPerGame",
                           "Date")
    
    dksalaries <- dksalaries[c("Date", 
                               "Name",
                               "NHL_Position",
                               "DK_Position",
                               "Salary", 
                               "AvgPointsPerGame")]
    
    #Add Games in Slate
    dksalaries["GamesInSlate"] <- dk_games
    
    #ADD DATA TO AGG FILE
    dk_salaries_final <- rbind(dksalaries, dk_salaries_final)
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #INCREMENT DAY BY 1
  dk_salaries_date <- dk_salaries_date + 1
  
}

#DELETE FROM AGG FILE WHERE PercDrafted is null
dk_salaries_final <- dk_salaries_final[!is.na(dk_salaries_final$Salary),]

dk_salaries_final <- dk_salaries_final[!dk_salaries_final$Salary == "",]



#WRITE TO DROPBOX
write.csv(dk_salaries_final,
          file = "C:/Users/jasselin/DataCamp Projects - Jupyter Notebooks/NHLData/2 - Data/Contest Player Salaries/dk_salaries_final.csv",
          row.names = FALSE
)


