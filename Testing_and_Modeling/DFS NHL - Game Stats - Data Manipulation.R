# Summarize skater and goalie statistics for easy use in training dataset

#Draw Required Libraries
library(data.table)
library(rvest)
library(XML)
library(xml2)
library(dplyr)
library(zoo)
library(stringdist)
library(rapportools)


#Read in most recent version of skater and goalie stats files
agg_skater_statistics <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/skater_statistics.csv",
                                  stringsAsFactors = FALSE)

agg_goalie_statistics <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/goalie_statistics.csv",
                                  stringsAsFactors = FALSE)


# Set Date as an actual date type
agg_goalie_statistics['Date'] <- lapply(agg_goalie_statistics['Date'], as.Date)
agg_skater_statistics['Date'] <- lapply(agg_skater_statistics['Date'], as.Date)


# ~~~~ TIME ON ICE MANIPULATION ~~~~~~ #

# ~~ SKATERS ~~ #

# Create vector(s) / column(s) so that I can format TimeOnIceMins properly
TimeOnIceMins_vector <- sapply(agg_skater_statistics["TimeOnIceMins"], strsplit, ":", "[", 2)

TimeOnIceMins_Mins <- vector()
TimeOnIceMins_Secs <- vector()

# Minutes
for (i in 1:length(TimeOnIceMins_vector)) {
  TimeOnIceMins_Mins[i] <- TimeOnIceMins_vector[[i]][1]
}

# Seconds
for (i in 1:length(TimeOnIceMins_vector)) {
  TimeOnIceMins_Secs[i] <- TimeOnIceMins_vector[[i]][2]
}

# Convert Seconds to numeric
TimeOnIceMins_Secs <- unlist(lapply(TimeOnIceMins_Secs, as.numeric))

# Convert Minutes to numeric
TimeOnIceMins_Mins <- unlist(lapply(TimeOnIceMins_Mins, as.numeric))

# Convert both to dataframes
# TimeOnIceMins_Secs <- as.data.frame(TimeOnIceMins_Secs)
# TimeOnIceMins_Mins <- as.data.frame(TimeOnIceMins_Mins)

# Add both as columns in the agg_skater_statistics df
agg_skater_statistics['TimeOnIce_Mins'] <- TimeOnIceMins_Mins
agg_skater_statistics['TimeOnIce_Secs'] <- TimeOnIceMins_Secs

# Convert Seconds column to minutes by dividing by 60
agg_skater_statistics['TimeOnIce_Secs'] <- agg_skater_statistics['TimeOnIce_Secs'] / 60

#Add new column with full TimeOnIce as a number
agg_skater_statistics['TimeOnIce_num'] <- round(agg_skater_statistics['TimeOnIce_Mins'] + agg_skater_statistics['TimeOnIce_Secs'],digits=2)

# Remove unnecessary columns
agg_skater_statistics <- agg_skater_statistics[c("Date",
                                                 "Player",
                                                 "Team",
                                                 "Opponent",
                                                 "Goals",
                                                 "Shots",
                                                 "ShotPerc",
                                                 "Assists",
                                                 "Points",
                                                 "Blocks",
                                                 "TimeOnIce_num",
                                                 "Shifts",
                                                 "PlusMinus",
                                                 "PenaltyMins",
                                                 "EvenStrengthGoals",
                                                 "EvenStrengthAssists",
                                                 "PowerPlayGoals",
                                                 "PowerPlayAssists",
                                                 "ShorthandedGoals",
                                                 "ShorthandedAssists",
                                                 "GameWinningGoals",
                                                 "Hits",
                                                 "FaceoffsWon",
                                                 "FaceoffsLost")]

#Rename TimeOnIce column
colnames(agg_skater_statistics)[colnames(agg_skater_statistics) == 'TimeOnIce_num'] <- 'TimeOnIceMins'

# ~~ DKPts Column
agg_skater_statistics <-  agg_skater_statistics %>%
  mutate(DK_Pts = (as.numeric(Goals) * 8.5) + 
           (as.numeric(Assists) * 5) + 
           (as.numeric(Shots) * 1.5) + 
           (as.numeric(Blocks) * 1.3) +
           (case_when(as.numeric(Shots) >= 5 ~ 3, TRUE ~ 0)) + 
           (case_when(as.numeric(Goals) >= 3 ~ 3, TRUE ~ 0)) +
           (case_when(as.numeric(Blocks) >= 3 ~ 3, TRUE ~ 0)) +
           (case_when((as.numeric(Goals) + as.numeric(Assists)) >= 3 ~ 3, TRUE ~ 0)),
         FaceoffsTaken = FaceoffsWon + FaceoffsLost,
         FaceoffWinPerc = FaceoffsWon / FaceoffsTaken
  )

# Replace new NaNs with 0
agg_skater_statistics[is.na(agg_skater_statistics)] <- 0

#Write file to CSV

write.csv(agg_skater_statistics,
          file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/skater_statistics_TimeOnIceFixedDKPtsAddedFaceoffsTakenandWinPercAdded.csv",
          row.names = FALSE
)



#Read in new file 

agg_skater_statistics <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/skater_statistics_TimeOnIceFixedDKPtsAddedFaceoffsTakenandWinPercAdded.csv",
          stringsAsFactors = FALSE
)


# Make the Date and Date and not a character
agg_skater_statistics$Date <- as.Date(agg_skater_statistics$Date)


# Player Stats Aggregations
# Want to roll up the last 1, 2, 3, ..., 20 games of data, by player, and by date, up to and before a given "slate" date
# This would allow for the data feeding the model to match any data that we would be able to pull prior to a given NHL slate

# Create a function to handle
# 1 game in the past function
hr_last_game_fn <- function(x_date, day_number) {
  skater_temp <- agg_skater_statistics[agg_skater_statistics$Date < x_date,]
  
  skater_ranks <-  agg_skater_statistics %>%
    group_by(Player) %>%
    mutate(my_ranks = order(Date, decreasing=TRUE))
  
  skater_x_games <- subset(skater_ranks, my_ranks <= day_number)
  
  skater_x_games <- skater_x_games %>%
    group_by(Player) %>%
    summarise(AvgGoalsPerGame_Lastx = round(mean(as.numeric(Goals)),2),
              AvgAssistsPerGame_Lastx = round(mean(as.numeric(Assists)),2),
              AvgShotsPerGame_Lastx = round(mean(as.numeric(Shots)),2),
              AvgBlocksPerGame_Lastx = round(mean(as.numeric(Blocks)),2),
              AvgTimeOnIcePerGame_Lastx = round(mean(as.numeric(TimeOnIceMins)),2),
              AvgDK_PtsPerGame_Lastx = round(mean(as.numeric(DK_Pts)),2)
    )
  
  skater_x_games["Date"] <- x_date
  
  skater_x_games["Days_Aggregated"] <- day_number
  
  return(skater_x_games)
}


# Create a file that has all "1 game" aggregations
start_date <- min(agg_skater_statistics$Date)
end_date <- max(agg_skater_statistics$Date)

skater_all_date_aggs <- data.frame("Date" = as.Date(character()),
                            "Player" = character(),
                            "Days_Aggregated" = numeric(),
                            "AvgGoalsPerGame_Last1" = numeric(),
                            "AvgAssistsPerGame_Last1" = numeric(),
                            "AvgShotsPerGame_Last1" = numeric(),
                            "AvgBlocksPerGame_Last1" = numeric(),
                            "AvgTimeOnIcePerGame_Last1" = numeric(),
                            "AvgDK_PtsPerGame_Last1" = numeric(), 
                            stringsAsFactors=FALSE)


for (x_games in 1:20) {
  
  while (start_date <= end_date) {
    start_date = start_date + 1
    
    skater_file_temp <- hr_last_game_fn(start_date, x_games)
    
    skater_all_date_aggs <- rbind(skater_all_date_aggs, skater_file_temp)
  }
  
  start_date <- min(agg_skater_statistics$Date)
  
  print(paste0(x_games," day aggregation finished.. Running ",x_games+1," days.."))
  
}

# Remove rows for which we cannot match a date from this file to the model data file (i.e. no slate data for a given night)?

# Read in one of the aggregated content files
#Read in new file 

dk_salaries_final <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Contest Player Salaries/dk_salaries_final.csv",
                                  stringsAsFactors = FALSE
)


# Make the Date and Date and not a character
dk_salaries_final$Date <- as.Date(dk_salaries_final$Date)


# Select all distinct dates from that table
dates_2_keep <- distinct(dk_salaries_final, Date)

# Only keep rows in skater_date_agg file where dates match the distinct dates above
skater_date_aggs_final <- skater_all_date_aggs[skater_all_date_aggs$Date %in% dates_2_keep$Date, ]


# Save skater_date_agg file to csv 
#Write file to CSV

write.csv(skater_date_aggs_final,
          file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/skater_date_aggs_final.csv",
          row.names = FALSE
)






# #MERGE TABLES INTO ONE TABLE
# skater_avgs <- merge(skater_this_season, skater_last_10games, by="Player", all.x=TRUE)
# 
# skater_avgs <- merge(skater_avgs, skater_last_5games, by="Player", all.x=TRUE)
# 
# skater_avgs <- merge(skater_avgs, skater_last_3games, by="Player", all.x=TRUE)
# 
# skater_avgs <- merge(skater_avgs, skater_last_game, by="Player", all.x=TRUE)
# 
# 
# #CALCULATE "MOMENTUM"/FLOOR/CEILING/VARIATION STATISTICS
# skater_avgs['DKPts_Season_25Band'] <- skater_avgs['AvgDK_PtsPerGame_Season'] - skater_avgs['stdDevDK_PtsPerGame_Season']
# 
# skater_avgs$DKPts_Season_25Band[skater_avgs$DKPts_Season_25Band < 0] <- 0
# 
# skater_avgs['DKPts_Season_75Band'] <- skater_avgs['AvgDK_PtsPerGame_Season'] + skater_avgs['stdDevDK_PtsPerGame_Season']
# 
# 
# 
# 
# 
