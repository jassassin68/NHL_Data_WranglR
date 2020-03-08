#draw required libraries
library(Rsymphony)
library(data.table)
library(plyr)
library(stringi)
library(rvest)
library(rstudioapi)
library(XML)
library(xml2)
library(dplyr)
library(magrittr)
library(neuralnet)
library(compare)
library(gtools)
library(fuzzyjoin)
require(compiler)
library(stringdist)
library(stringr)
library(rlist)
library(utils)
library(httr)
library(tidyr)
library(ade4)
library(caret)
#library(shiny)
#library(shinydashboard)
enableJIT(3)
library(moments)

#Read in all data; create "standard" dataset
# DK Salary
dk_salaries_agg <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Contest Player salaries/dk_salaries_final.csv",
                            stringsAsFactors = FALSE)

#Contest Results - GPP
player_perc_drafted_gpp <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Contest Ownership/player_perc_drafted_gpp.csv",
                                    stringsAsFactors = FALSE)

#Contest Results - Cash
player_perc_drafted_cash <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Contest Ownership/player_perc_drafted_cash.csv",
                                     stringsAsFactors = FALSE)

#Player Data
player_file_agg <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/Agg Player Data/player_data_agg v2.csv",
                            stringsAsFactors = FALSE)


# Player Stats
skater_date_aggs <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/skater_date_aggs_final.csv",
                             stringsAsFactors = FALSE)




#Create empty dataframe with proper columns, merge tables, then merge with final
nhl_training_data_final <- data.frame(Date = as.Date(character()),
                                      Name = as.character(),
                                      DK_Position = as.character(),
                                      Salary = as.numeric(),
                                      Line = as.character(),
                                      PP_Line = as.character(),
                                      #  Impact_Player_Flag = as.numeric(),
                                      Implied_goals = as.numeric(),
                                      AvgPointsPerGame = as.numeric(),
                                      GamesInSlate = as.numeric(),
                                      PercDraftedCash = as.numeric(),
                                      PercDraftedGPP = as.numeric(),
                                      DKPts = as.numeric(),
                                      stringsAsFactors = FALSE)

nhl_training_data <- merge(dk_salaries_agg, player_perc_drafted_cash, by = c("Date", "Name"))

nhl_training_data <- merge(nhl_training_data, player_perc_drafted_gpp, by = c("Date", "Name"))

nhl_training_data <- merge(nhl_training_data, player_file_agg, by = c("Date", "Name"))

#nhl_training_data <- merge(nhl_training_data, all_star_roster_players, by = "Name", all.x = TRUE)

#Select non-duplicate columns
nhl_training_data <- nhl_training_data[c("Date",
                                         "Name",
                                         "DK_Position",
                                         "Salary",
                                         "Line",
                                         "PP_Line",
                                         "Implied_goals",
                                         "AvgPointsPerGame",
                                         "GamesInSlate",
                                         "PercDrafted.x",
                                         "PercDrafted.y",
                                         "DKPts.x")]


#Re-Name columns
names(nhl_training_data) <- c("Date",
                              "Name",
                              "Position",
                              "Salary",
                              "Line",
                              "PP_Line",
                              "Implied_goals",
                              "AvgPointsPerGame",
                              "GamesInSlate",
                              "PercDrafted_Cash",
                              "PercDrafted_GPP",
                              "DKPts")

# Delete Goalies
nhl_training_data <- nhl_training_data[nhl_training_data['Position'] != 'G',]

#Merge with final data table
nhl_training_data_final <- rbind(nhl_training_data_final, nhl_training_data)


#THINGS TO DO:
#   ADJUST DATA TYPES (PercDrafted (Both), Date, Name, Position, Line, PP_Line if needed)
# All set on PercDrafted, don't need to change rest since won't be input into model
nhl_training_data_final[is.na(nhl_training_data_final)] <- 0

#Make list of variable names for which we want to create a one-hot encoding column
nhl_binary_list <- c("Line", "PP_Line", "Position") #Not going to encode Position for now, may add if I think it will help

#Loop thru list to create dummy variables
for (f in nhl_binary_list){
  df_all_dummy = acm.disjonctif(nhl_training_data_final[f])
  nhl_training_data_final[f] = NULL
  nhl_training_data_final = cbind(nhl_training_data_final, df_all_dummy)
}

# Rename columns
names(nhl_training_data_final) <- c("Date",
                                "Name",
                                "Salary",
                                "Implied_goals",
                                "AvgPointsPerGame",
                                "GamesInSlate",
                                "PercDrafted_Cash",
                                "PercDrafted_GPP",
                                "DKPts",
                                "Line.D1",
                                "Line.D2",
                                "Line.D3",
                                "Line.Line1",
                                "Line.Line2",
                                "Line.Line3",
                                "Line.Line4",
                                "PP_Line.0",
                                "PP_Line.PP1",
                                "PP_Line.PP2",
                                "Position.C",
                                "Position.D",
                                "Position.W")

# Review skewness of dependent variables
# print(skewness(nhl_training_data_final$DKPts, na.rm = TRUE))
# print(skewness(nhl_training_data_final$PercDrafted_Cash, na.rm = TRUE))
# print(skewness(nhl_training_data_final$PercDrafted_GPP, na.rm = TRUE))
#print(skewness(log(nhl_training_data_final$Salary), na.rm = TRUE)) - **POTENTIAL IMPROVEMENT: LOG(SALARY) AS INPUT
# 
# # Try square root and log10 functions to see if that improves skewness (i.e. closer to 0)
# # sqrt
# print(skewness(sqrt(nhl_training_data_final$DKPts), na.rm = TRUE))
# print(skewness(sqrt(nhl_training_data_final$PercDrafted_Cash), na.rm = TRUE))
# print(skewness(sqrt(nhl_training_data_final$PercDrafted_GPP), na.rm = TRUE))
# # log
# print(skewness(log(nhl_training_data_final$DKPts), na.rm = TRUE))
# print(skewness(log(nhl_training_data_final$PercDrafted_Cash), na.rm = TRUE))
# print(skewness(log(nhl_training_data_final$PercDrafted_GPP), na.rm = TRUE))
# 

# Conclusion:
#   DKPts - Use square root to normalize; then will need to square prediction
#   PercDrafted - Use log to normalize; then will need to raise prediction to e

nhl_training_data_final$DKPts <- sqrt(nhl_training_data_final$DKPts)
nhl_training_data_final$PercDrafted_Cash <- log(nhl_training_data_final$PercDrafted_Cash)
nhl_training_data_final$PercDrafted_GPP <- log(nhl_training_data_final$PercDrafted_GPP)


# Create subset "training_data_final" table with just one of the dependent variables we want
# DKPts
nhl_training_dkpts <- nhl_training_data_final[c("Date",
                                                "Name",
                                                "Salary",
                                                "Implied_goals",
                                                "GamesInSlate",
                                                "DKPts",
                                                "Line.D1",
                                                "Line.D2",
                                                "Line.D3",
                                                "Line.Line1",
                                                "Line.Line2",
                                                "Line.Line3",
                                                "Line.Line4",
                                                "PP_Line.0",
                                                "PP_Line.PP1",
                                                "PP_Line.PP2",
                                                "Position.C",
                                                "Position.D",
                                                "Position.W")]

df_results <- data.frame(Var_Predicted = character(),
                         Iteration = numeric(),
                         RSquared = numeric(),
                         RSS = numeric(),
                         MSE = numeric(),
                         RMSE = numeric())

# Create function or loop that tries each of the date aggs in a linear model, 1-20,
# And returns the output of the model (RMSE and R-Squared?) for each run
# Create a function to handle
# 1 game in the past function
dk_pts_model_iter_fn <- function(day_number) {
  skater_temp <- skater_date_aggs[skater_date_aggs$Days_Aggregated == day_number,]
  
  skater_full <- merge(nhl_training_dkpts, skater_temp, by.x = c("Name","Date"), by.y = c("Player","Date"))
  
  # Removed due to testing: GamesInSlate,allDLines,allPositions,AvgGoals,Line3,Line4,PP2
  skater_full2 <- skater_full[c("Salary",
                               "Implied_goals",
                               "Line.Line1",
                               "Line.Line2",
                               "PP_Line.0",
                               "PP_Line.PP1",
                               "AvgAssistsPerGame_Lastx",
                               "AvgShotsPerGame_Lastx",
                               "AvgBlocksPerGame_Lastx",
                               "AvgTimeOnIcePerGame_Lastx",
                               "DKPts"
                               )]
  
  # Define Independent variable columns
  cols <- c("Salary",
            "Implied_goals",
            "Line.Line1",
            "Line.Line2",
            "PP_Line.0",
            "PP_Line.PP1",
            "AvgAssistsPerGame_Lastx",
            "AvgShotsPerGame_Lastx",
            "AvgBlocksPerGame_Lastx",
            "AvgTimeOnIcePerGame_Lastx")
  
  # Create input
  cn <- paste(cols,collapse = ' + ')
  
  # Run Model
  nhl_lm <- lm(formula = paste("DKPts ~ ",cn), data = skater_full2)
  
  # Store off r-squared, RSS, MSE, and RMSE of model run
  r_2 <- summary(nhl_lm)$adj.r.squared
  RSS <- c(crossprod(nhl_lm$residuals))
  MSE <- RSS / length(nhl_lm$residuals)
  RMSE <- sqrt(MSE)
  
  # Insert into data.frame
  tmp <- data.frame(Var_Predicted = "DKPts",
                    Iteration = day_number,
                    RSquared = r_2,
                    RSS = RSS,
                    MSE = MSE,
                    RMSE = RMSE)
  
  df_results <- rbind(df_results,tmp)
  
  print(df_results)
}


# Run function through loop
for (i in 1:20) {
  dk_pts_model_iter_fn(i)
}


# Conclusion: 7 Days is the best look-back date aggregation for modeling DKPts


# PercDrafted_Cash
nhl_training_cash <- nhl_training_data_final[c("Date",
                                                "Name",
                                                "Salary",
                                                "Implied_goals",
                                                "GamesInSlate",
                                                "PercDrafted_Cash",
                                                "Line.D1",
                                                "Line.D2",
                                                "Line.D3",
                                                "Line.Line1",
                                                "Line.Line2",
                                                "Line.Line3",
                                                "Line.Line4",
                                                "PP_Line.0",
                                                "PP_Line.PP1",
                                                "PP_Line.PP2",
                                                "Position.C",
                                                "Position.D",
                                                "Position.W")]

# Create function or loop that tries each of the date aggs in a linear model, 1-20,
# And returns the output of the model (RMSE and R-Squared?) for each run
# Create a function to handle
# 1 game in the past function
dk_cash_iter_fn <- function(day_number) {
  skater_temp <- skater_date_aggs[skater_date_aggs$Days_Aggregated == day_number,]
  
  skater_full <- merge(nhl_training_cash, skater_temp, by.x = c("Name","Date"), by.y = c("Player","Date"))
  
  # Removed due to testing: AvgGoals,Line4,PP2,Position.W
  skater_full2 <- skater_full[c("Salary",
                                "Implied_goals",
                                "GamesInSlate",
                                "Line.D1",
                                "Line.D2",
                                "Line.D3",
                                "Line.Line1",
                                "Line.Line2",
                                "Line.Line3",
                                "PP_Line.0",
                                "PP_Line.PP1",
                                "Position.C",
                                "Position.D",
                                "AvgAssistsPerGame_Lastx",
                                "AvgShotsPerGame_Lastx",
                                "AvgBlocksPerGame_Lastx",
                                "AvgTimeOnIcePerGame_Lastx",
                                "PercDrafted_Cash"
  )]
  
  # Define Independent variable columns
  cols <- c("Salary",
            "Implied_goals",
            "GamesInSlate",
            "Line.D1",
            "Line.D2",
            "Line.D3",
            "Line.Line1",
            "Line.Line2",
            "Line.Line3",
            "PP_Line.0",
            "PP_Line.PP1",
            "Position.C",
            "Position.D",
            "AvgAssistsPerGame_Lastx",
            "AvgShotsPerGame_Lastx",
            "AvgBlocksPerGame_Lastx",
            "AvgTimeOnIcePerGame_Lastx")
  
  # Create input
  cn <- paste(cols,collapse = ' + ')
  
  # Run Model
  nhl_lm <- lm(formula = paste("PercDrafted_Cash ~ ",cn), data = skater_full2)
  
  # Store off r-squared, RSS, MSE, and RMSE of model run
  r_2 <- summary(nhl_lm)$adj.r.squared
  RSS <- c(crossprod(nhl_lm$residuals))
  MSE <- RSS / length(nhl_lm$residuals)
  RMSE <- sqrt(MSE)
  
  # Insert into data.frame
  tmp <- data.frame(Var_Predicted = "PercDrafted_Cash",
                    Iteration = day_number,
                    RSquared = r_2,
                    RSS = RSS,
                    MSE = MSE,
                    RMSE = RMSE)
  
  df_results <- rbind(df_results,tmp)
  
  print(df_results)
}


# Run function through loop
for (i in 1:20) {
  dk_cash_iter_fn(i)
}


# Conclusion: 15 days is the best look-back period for modeling PercDrafted_Cash




# PercDrafted_GPP
nhl_training_gpp <- nhl_training_data_final[c("Date",
                                               "Name",
                                               "Salary",
                                               "Implied_goals",
                                               "GamesInSlate",
                                               "PercDrafted_GPP",
                                               "Line.D1",
                                               "Line.D2",
                                               "Line.D3",
                                               "Line.Line1",
                                               "Line.Line2",
                                               "Line.Line3",
                                               "Line.Line4",
                                               "PP_Line.0",
                                               "PP_Line.PP1",
                                               "PP_Line.PP2",
                                               "Position.C",
                                               "Position.D",
                                               "Position.W")]

# Create function or loop that tries each of the date aggs in a linear model, 1-20,
# And returns the output of the model (RMSE and R-Squared?) for each run
# Create a function to handle
# 1 game in the past function
dk_gpp_iter_fn <- function(day_number) {
  skater_temp <- skater_date_aggs[skater_date_aggs$Days_Aggregated == day_number,]
  
  skater_full <- merge(nhl_training_gpp, skater_temp, by.x = c("Name","Date"), by.y = c("Player","Date"))
  
  # Removed due to testing: AvgGoals,Line4,PP2,Position.W,Position.C
  skater_full2 <- skater_full[c("Salary",
                                "Implied_goals",
                                "GamesInSlate",
                                "Line.D1",
                                "Line.D2",
                                "Line.D3",
                                "Line.Line1",
                                "Line.Line2",
                                "Line.Line3",
                                "PP_Line.0",
                                "PP_Line.PP1",
                                "Position.D",
                                "AvgAssistsPerGame_Lastx",
                                "AvgShotsPerGame_Lastx",
                                "AvgBlocksPerGame_Lastx",
                                "AvgTimeOnIcePerGame_Lastx",
                                "PercDrafted_GPP"
  )]
  
  # Define Independent variable columns
  cols <- c("Salary",
            "Implied_goals",
            "GamesInSlate",
            "Line.D1",
            "Line.D2",
            "Line.D3",
            "Line.Line1",
            "Line.Line2",
            "Line.Line3",
            "PP_Line.0",
            "PP_Line.PP1",
            "Position.D",
            "AvgAssistsPerGame_Lastx",
            "AvgShotsPerGame_Lastx",
            "AvgBlocksPerGame_Lastx",
            "AvgTimeOnIcePerGame_Lastx")
  
  # Create input
  cn <- paste(cols,collapse = ' + ')
  
  # Run Model
  nhl_lm <- lm(formula = paste("PercDrafted_GPP ~ ",cn), data = skater_full2)
  
  # Store off r-squared, RSS, MSE, and RMSE of model run
  r_2 <- summary(nhl_lm)$adj.r.squared
  RSS <- c(crossprod(nhl_lm$residuals))
  MSE <- RSS / length(nhl_lm$residuals)
  RMSE <- sqrt(MSE)
  
  # Insert into data.frame
  tmp <- data.frame(Var_Predicted = "PercDrafted_GPP",
                    Iteration = day_number,
                    RSquared = r_2,
                    RSS = RSS,
                    MSE = MSE,
                    RMSE = RMSE)
  
  df_results <- rbind(df_results,tmp)
  
  print(df_results)
}


# Run function through loop
for (i in 1:20) {
  dk_gpp_iter_fn(i)
}


# Conclusion: 15 days is the best look-back period for modeling PercDrafted_GPP




# Final models
# DKPts - 7 day look-back on stats
nhl_training_dkpts <- nhl_training_data_final[c("Date",
                                                "Name",
                                                "Salary",
                                                "Implied_goals",
                                                "GamesInSlate",
                                                "DKPts",
                                                "Line.D1",
                                                "Line.D2",
                                                "Line.D3",
                                                "Line.Line1",
                                                "Line.Line2",
                                                "Line.Line3",
                                                "Line.Line4",
                                                "PP_Line.0",
                                                "PP_Line.PP1",
                                                "PP_Line.PP2",
                                                "Position.C",
                                                "Position.D",
                                                "Position.W")]

skater_temp <- skater_date_aggs[skater_date_aggs$Days_Aggregated == 7,]
  
skater_full <- merge(nhl_training_dkpts, skater_temp, by.x = c("Name","Date"), by.y = c("Player","Date"))

# Removed due to testing: GamesInSlate,allDLines,allPositions,AvgGoals,Line3,Line4,PP2
skater_full2 <- skater_full[c("Salary",
                              "Implied_goals",
                              "Line.Line1",
                              "Line.Line2",
                              "PP_Line.0",
                              "PP_Line.PP1",
                              "AvgAssistsPerGame_Lastx",
                              "AvgShotsPerGame_Lastx",
                              "AvgBlocksPerGame_Lastx",
                              "AvgTimeOnIcePerGame_Lastx",
                              "DKPts"
)]

# Define Independent variable columns
cols <- c("Salary",
          "Implied_goals",
          "Line.Line1",
          "Line.Line2",
          "PP_Line.0",
          "PP_Line.PP1",
          "AvgAssistsPerGame_Lastx",
          "AvgShotsPerGame_Lastx",
          "AvgBlocksPerGame_Lastx",
          "AvgTimeOnIcePerGame_Lastx")

# Create input
cn <- paste(cols,collapse = ' + ')

# Run Model
nhl_lm <- lm(formula = paste("DKPts ~ ",cn), data = skater_full2)

# Store off r-squared, RSS, MSE, and RMSE of model run
r_2 <- summary(nhl_lm)$adj.r.squared
RSS <- c(crossprod(nhl_lm$residuals))
MSE <- RSS / length(nhl_lm$residuals)
RMSE <- sqrt(MSE)

# Insert into data.frame
tmp <- data.frame(Var_Predicted = "DKPts",
                  Iteration = 7,
                  RSquared = r_2,
                  RSS = RSS,
                  MSE = MSE,
                  RMSE = RMSE)

# Write results to csv
write.csv(as.data.frame(summary(nhl_lm)$coef), 
          file="C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Model Results/dkpts_regression.csv"
          )




# PercDrafted_Cash
nhl_training_cash <- nhl_training_data_final[c("Date",
                                               "Name",
                                               "Salary",
                                               "Implied_goals",
                                               "GamesInSlate",
                                               "PercDrafted_Cash",
                                               "Line.D1",
                                               "Line.D2",
                                               "Line.D3",
                                               "Line.Line1",
                                               "Line.Line2",
                                               "Line.Line3",
                                               "Line.Line4",
                                               "PP_Line.0",
                                               "PP_Line.PP1",
                                               "PP_Line.PP2",
                                               "Position.C",
                                               "Position.D",
                                               "Position.W")]

skater_temp <- skater_date_aggs[skater_date_aggs$Days_Aggregated == 15,]

skater_full <- merge(nhl_training_cash, skater_temp, by.x = c("Name","Date"), by.y = c("Player","Date"))

# Removed due to testing: AvgGoals,Line4,PP2,Position.W
skater_full2 <- skater_full[c("Salary",
                              "Implied_goals",
                              "GamesInSlate",
                              "Line.D1",
                              "Line.D2",
                              "Line.D3",
                              "Line.Line1",
                              "Line.Line2",
                              "Line.Line3",
                              "PP_Line.0",
                              "PP_Line.PP1",
                              "Position.C",
                              "Position.D",
                              "AvgAssistsPerGame_Lastx",
                              "AvgShotsPerGame_Lastx",
                              "AvgBlocksPerGame_Lastx",
                              "AvgTimeOnIcePerGame_Lastx",
                              "PercDrafted_Cash"
)]

# Define Independent variable columns
cols <- c("Salary",
          "Implied_goals",
          "GamesInSlate",
          "Line.D1",
          "Line.D2",
          "Line.D3",
          "Line.Line1",
          "Line.Line2",
          "Line.Line3",
          "PP_Line.0",
          "PP_Line.PP1",
          "Position.C",
          "Position.D",
          "AvgAssistsPerGame_Lastx",
          "AvgShotsPerGame_Lastx",
          "AvgBlocksPerGame_Lastx",
          "AvgTimeOnIcePerGame_Lastx")

# Create input
cn <- paste(cols,collapse = ' + ')

# Run Model
nhl_lm <- lm(formula = paste("PercDrafted_Cash ~ ",cn), data = skater_full2)

# Store off r-squared, RSS, MSE, and RMSE of model run
r_2 <- summary(nhl_lm)$adj.r.squared
RSS <- c(crossprod(nhl_lm$residuals))
MSE <- RSS / length(nhl_lm$residuals)
RMSE <- sqrt(MSE)

# Insert into data.frame
tmp <- data.frame(Var_Predicted = "PercDrafted_Cash",
                  Iteration = 15,
                  RSquared = r_2,
                  RSS = RSS,
                  MSE = MSE,
                  RMSE = RMSE)

# Write results to csv
write.csv(as.data.frame(summary(nhl_lm)$coef), 
          file="C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Model Results/PercDraftedCash_regression.csv"
)



# PercDrafted_GPP
nhl_training_gpp <- nhl_training_data_final[c("Date",
                                              "Name",
                                              "Salary",
                                              "Implied_goals",
                                              "GamesInSlate",
                                              "PercDrafted_GPP",
                                              "Line.D1",
                                              "Line.D2",
                                              "Line.D3",
                                              "Line.Line1",
                                              "Line.Line2",
                                              "Line.Line3",
                                              "Line.Line4",
                                              "PP_Line.0",
                                              "PP_Line.PP1",
                                              "PP_Line.PP2",
                                              "Position.C",
                                              "Position.D",
                                              "Position.W")]


skater_temp <- skater_date_aggs[skater_date_aggs$Days_Aggregated == 15,]

skater_full <- merge(nhl_training_gpp, skater_temp, by.x = c("Name","Date"), by.y = c("Player","Date"))

# Removed due to testing: AvgGoals,Line4,PP2,Position.W,Position.C
skater_full2 <- skater_full[c("Salary",
                              "Implied_goals",
                              "GamesInSlate",
                              "Line.D1",
                              "Line.D2",
                              "Line.D3",
                              "Line.Line1",
                              "Line.Line2",
                              "Line.Line3",
                              "PP_Line.0",
                              "PP_Line.PP1",
                              "Position.D",
                              "AvgAssistsPerGame_Lastx",
                              "AvgShotsPerGame_Lastx",
                              "AvgBlocksPerGame_Lastx",
                              "AvgTimeOnIcePerGame_Lastx",
                              "PercDrafted_GPP"
)]

# Define Independent variable columns
cols <- c("Salary",
          "Implied_goals",
          "GamesInSlate",
          "Line.D1",
          "Line.D2",
          "Line.D3",
          "Line.Line1",
          "Line.Line2",
          "Line.Line3",
          "PP_Line.0",
          "PP_Line.PP1",
          "Position.D",
          "AvgAssistsPerGame_Lastx",
          "AvgShotsPerGame_Lastx",
          "AvgBlocksPerGame_Lastx",
          "AvgTimeOnIcePerGame_Lastx")

# Create input
cn <- paste(cols,collapse = ' + ')

# Run Model
nhl_lm <- lm(formula = paste("PercDrafted_GPP ~ ",cn), data = skater_full2)

# Store off r-squared, RSS, MSE, and RMSE of model run
r_2 <- summary(nhl_lm)$adj.r.squared
RSS <- c(crossprod(nhl_lm$residuals))
MSE <- RSS / length(nhl_lm$residuals)
RMSE <- sqrt(MSE)

# Insert into data.frame
tmp <- data.frame(Var_Predicted = "PercDrafted_GPP",
                  Iteration = 15,
                  RSquared = r_2,
                  RSS = RSS,
                  MSE = MSE,
                  RMSE = RMSE)



# Write results to csv
write.csv(as.data.frame(summary(nhl_lm)$coef), 
          file="C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Model Results/PercDraftedGPP_regression.csv"
)



