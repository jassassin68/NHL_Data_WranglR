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



#~~~~~~~~STEP 0: COMBINE "YESTERDAY"'S PLAYER FILE WITH "YESTERDAY"'S BOX SCORE STATS AND CONTEST RESULTS FILE~~~~~~~~##

#BOX SCORE STATS
#HOCKEY REFERENCE DATA
#GET DATE AND DECLARE DATE VARIABLES
today <-  Sys.Date()


today_day <- substr(today, 9, 10)
today_month <- substr(today, 6, 7)
today_year <- substr(today, 1, 4)
yesterday_day <- substr(today-1, 9, 10)
yesterday_month <- substr(today-1, 6, 7)
yesterday_year <- substr(today-1, 1, 4)


#Read in most recent version of skater and goalie stats files
agg_skater_statistics <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/skater_statistics.csv")

agg_goalie_statistics <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/goalie_statistics.csv")

#Find Max Date in skater and goalie files (should match, if not raise error)
max_skater_date <- max(as.Date(agg_skater_statistics$Date))
max_goalie_date <- max(as.Date(agg_goalie_statistics$Date))

if (max_skater_date != max_goalie_date) {
  stop()
}

#Loop through all missing dates so that the agg_statistics files are caught up to most recent completed date (yesterday)

hockey_ref_date <- max_skater_date + 1

while (hockey_ref_date < today) {
  
  #Create day, month, year variables
  hockey_ref_date_day <- substr(hockey_ref_date, 9, 10)
  hockey_ref_date_month <- substr(hockey_ref_date, 6, 7)
  hockey_ref_date_year <- substr(hockey_ref_date, 1, 4)
  
  
  #CONSTRUCT URL FOR YESTERDAY'S HOCKEY-REFERENCE DATA URL
  hockey_ref_url <- paste0("https://www.hockey-reference.com/friv/dailyleaders.fcgi?year=",
                           toString(hockey_ref_date_year),
                           "&month=",
                           toString(hockey_ref_date_month),
                           "&day=",
                           toString(hockey_ref_date_day)
  )
  
  Sys.sleep(time = 3)
  #READ IN THE URL
  hockey_ref <- read_html(hockey_ref_url)
  
  Sys.sleep(time = 3)
  #PULL ALL PLAYERS FROM YESTERDAY'S HOCKEY-REFERENCE DATA URL
  hockey_ref_players <- html_text(html_nodes(hockey_ref, ".left:nth-child(2) a"))
  
  Sys.sleep(time = 3)
  #PULL COUNT OF SKATERS FROM YESTERDAY'S HOCKEY-REFERENCE DATA URL
  hockey_ref_num_skaters <- html_text(html_nodes(hockey_ref, "#all_skaters h2"))
  
  #CONVERT TO NUMBER
  tryCatch({
    hockey_ref_num_skaters <- as.numeric(substr(hockey_ref_num_skaters, 
                                                1, 
                                                nchar(hockey_ref_num_skaters) - 8
    )
    )},error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  if (is.empty(hockey_ref_num_skaters) == FALSE) {
    
    #SPLIT UP PLAYER LIST INTO SKATERS AND GOALIES BASED ON SKATER COUNT
    hockey_ref_skaters <- hockey_ref_players[1:hockey_ref_num_skaters]
    
    hockey_ref_goalies <- hockey_ref_players[(hockey_ref_num_skaters + 1):length(hockey_ref_players)]
    
    
    #PULL ALL STATS FROM YESTERDAY'S HOCKEY-REFERENCE DATA URL
    Sys.sleep(time = 3)
    hockey_ref_stats <- html_text(html_nodes(hockey_ref, "td.right"))
    
    #REMOVE 'BOXSCORE' COLUMNS FROM STATS LIST
    hockey_ref_stats <- hockey_ref_stats[hockey_ref_stats != "boxscore"]
    
    #REPLACE ANY BLANKS WITH 0
    hockey_ref_stats[hockey_ref_stats == ""] <- 0
    
    
    #PULL ALL COLUMN NAMES FROM YESTERDAY'S HOCKEY-REFERENCE DATA URL
    Sys.sleep(time = 3)
    hockey_ref_colnames <- html_text(html_nodes(hockey_ref, ".poptip"))
    
    #DECLARE SKATER COLUMNS
    hockey_ref_skater_colnames <- hockey_ref_colnames[9:28]
    
    
    #CREATE DATAFRAME OF SKATER STATS
    hockey_ref_stats_by_skater <- split(hockey_ref_stats, ceiling(seq_along(hockey_ref_stats)/20))
    
    hockey_ref_stats_by_skater <- do.call(rbind, hockey_ref_stats_by_skater)
    
    hockey_ref_stats_by_skater <- data.frame(hockey_ref_stats_by_skater[1:hockey_ref_num_skaters,], stringsAsFactors = FALSE)
    
    #SET COLUMN NAMES
    colnames(hockey_ref_stats_by_skater) <- hockey_ref_skater_colnames
    
    #ADD PLAYER COLUMN
    hockey_ref_stats_by_skater['Player'] <- hockey_ref_skaters
    
    #ADD DATE COLUMN
    hockey_ref_stats_by_skater['Date'] <- hockey_ref_date
    
    #RENAME COLUMNS
    setnames(hockey_ref_stats_by_skater, old=c("G","A","PTS","+/-","PIM",
                                               "EV","PP","SH","GW",
                                               "EV.1","PP.1","SH.1",
                                               "S","S%","SHFT","TOI",
                                               "HIT","BLK","FOW","FOL"), 
             new=c("Goals","Assists","Points","PlusMinus","PenaltyMins",
                   "EvenStrengthGoals","PowerPlayGoals","ShorthandedGoals",
                   "GameWinningGoals","EvenStrengthAssists","PowerPlayAssists",
                   "ShorthandedAssists","Shots","ShotPerc","Shifts","TimeOnIceMins",
                   "Hits","Blocks","FaceoffsWon","FaceoffsLost")
    )
    
    #PULL PLAYER TEAM AND OPPONENT DATA
    Sys.sleep(time = 3)
    hockey_ref_players2 <- html_text(html_nodes(hockey_ref, "#skaters a"))
    
    #REMOVE 'BOXSCORE' COLUMNS FROM PLAYER TEAM LIST
    hockey_ref_players2 <- hockey_ref_players2[hockey_ref_players2 != "boxscore"]
    
    #CREATE DATAFRAME OF PLAYER TEAM AND OPPONENT DATA
    hockey_ref_player_team <- split(hockey_ref_players2, ceiling(seq_along(hockey_ref_players2)/3))
    
    hockey_ref_player_team <- do.call(rbind, hockey_ref_player_team)
    
    colnames(hockey_ref_player_team) <- c("Player", "Team", "Opponent")
    
    #MERGE PLAYER STAT AND TEAM DATA TABLES
    hockey_ref_stats_by_skater <- merge(hockey_ref_stats_by_skater, hockey_ref_player_team, by = "Player")
    
    
    #REORDER COLUMNS
    hockey_ref_stats_by_skater <- hockey_ref_stats_by_skater[c("Date",
                                                               "Player",
                                                               "Team",
                                                               "Opponent",
                                                               "Goals",
                                                               "Assists",
                                                               "Points",
                                                               "Shots",
                                                               "Blocks",
                                                               "TimeOnIceMins",
                                                               "ShotPerc",
                                                               "Shifts",
                                                               "PlusMinus",
                                                               "PenaltyMins",
                                                               "EvenStrengthGoals",
                                                               "PowerPlayGoals",
                                                               "ShorthandedGoals",
                                                               "GameWinningGoals",
                                                               "EvenStrengthAssists",
                                                               "PowerPlayAssists",
                                                               "ShorthandedAssists",
                                                               "Hits",
                                                               "FaceoffsWon",
                                                               "FaceoffsLost"
    )
    ]
    
    #CHANGE FORMAT OF TIME ON ICE
    hockey_ref_stats_by_skater["TimeOnIceMins"] <- lapply(hockey_ref_stats_by_skater["TimeOnIceMins"], format, format = "%M")
    
    #FILL IN ANY NAs OR BLANKS WITH 0
    hockey_ref_stats_by_skater[is.na(hockey_ref_stats_by_skater)] <- 0
    
  } else {
    #Add Day
    hockey_ref_date <- hockey_ref_date + 1
    next
  }
  
  
  #~~~~~~~~~~~~~~~GOALIES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  #CREATE DATAFRAME OF GOALIE STATS
  hockey_ref_stats_by_goalie <- tail(hockey_ref_stats, -(hockey_ref_num_skaters * 20))
  
  hockey_ref_stats_by_goalie <- split(hockey_ref_stats_by_goalie, ceiling(seq_along(hockey_ref_stats_by_goalie) / 9))
  
  hockey_ref_stats_by_goalie <- data.frame(do.call(rbind, hockey_ref_stats_by_goalie), stringsAsFactors = FALSE)
  
  
  #CREATE GOALIE COLUMNS LIST
  hockey_ref_goalie_colnames <- hockey_ref_colnames[39:47]
  
  #ADD COLUMN NAMES TO DF
  colnames(hockey_ref_stats_by_goalie) <- hockey_ref_goalie_colnames
  
  #ADD PLAYER COLUMN
  hockey_ref_stats_by_goalie['Player'] <- hockey_ref_goalies
  
  #ADD DATE COLUMN
  hockey_ref_stats_by_goalie['Date'] <- hockey_ref_date
  
  #RENAME COLUMNS
  setnames(hockey_ref_stats_by_goalie, old=c("GA", "SA", "SV", "SV%", "SO", "G", "A", "PIM", "TOI"), 
           new=c("GoalsAllowed", "ShotsAllowed", "Saves", "SavePerc", 
                 "ShutoutFlag", "Goals", "Assists", "PenaltyMins", "TimeOnIceMins")
  )
  
  #PULL PLAYER TEAM AND OPPONENT DATA
  Sys.sleep(time = 3)
  hockey_ref_goalies2 <- html_text(html_nodes(hockey_ref, "#goalies a"))
  
  #REMOVE 'BOXSCORE' COLUMNS FROM PLAYER TEAM LIST
  hockey_ref_goalies2 <- hockey_ref_goalies2[hockey_ref_goalies2 != "boxscore"]
  
  #CREATE DATAFRAME OF PLAYER TEAM AND OPPONENT DATA
  hockey_ref_goalie_team <- split(hockey_ref_goalies2, ceiling(seq_along(hockey_ref_goalies2)/3))
  
  hockey_ref_goalie_team <- do.call(rbind, hockey_ref_goalie_team)
  
  colnames(hockey_ref_goalie_team) <- c("Player", "Team", "Opponent")
  
  #MERGE PLAYER STAT AND TEAM DATA TABLES
  hockey_ref_stats_by_goalie <- merge(hockey_ref_stats_by_goalie, hockey_ref_goalie_team, by = "Player")
  
  
  #REORDER COLUMNS
  hockey_ref_stats_by_goalie <- hockey_ref_stats_by_goalie[c("Date",
                                                             "Player",
                                                             "Team",
                                                             "Opponent",
                                                             "GoalsAllowed", 
                                                             "ShotsAllowed", 
                                                             "Saves", 
                                                             "SavePerc", 
                                                             "ShutoutFlag",
                                                             "Goals",
                                                             "Assists",
                                                             "PenaltyMins", 
                                                             "TimeOnIceMins"
  )
  ]
  
  #CHANGE FORMAT OF TIME ON ICE
  hockey_ref_stats_by_goalie["TimeOnIceMins"] <- lapply(hockey_ref_stats_by_goalie["TimeOnIceMins"], format, format = "%M")
  
  #FILL IN ANY NAs OR BLANKS WITH 0
  hockey_ref_stats_by_goalie[is.na(hockey_ref_stats_by_goalie)] <- 0
  
  
  
  #~~~~~~~~~~~~~AGGREGATE DATA~~~~~~~~~~~~~~
  
  
  #WRITE FILES TO CSV AND COMBINE WITH AGG FILE
  #SKATERS
  
  agg_skater_statistics <- rbind(hockey_ref_stats_by_skater, agg_skater_statistics)
  
  
  #GOALIES
  
  agg_goalie_statistics <- rbind(hockey_ref_stats_by_goalie, agg_goalie_statistics)
  
  
  #Add Day
  hockey_ref_date <- hockey_ref_date + 1
  
}


#Write files to CSV

write.csv(agg_skater_statistics,
          file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/skater_statistics.csv",
          row.names = FALSE
)

write.csv(agg_goalie_statistics,
          file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/goalie_statistics.csv",
          row.names = FALSE
)



