#Explore NHL Training Dataset
#Have:
#   All Contest Data (Player, Date, Ownership %, DK Pts) aggregated
#   All DK Salary Data (Player, Date, Position, Salary, Games in Slate, implied_goals) aggregated
#  ** BELOW DON'T CURRENT HAVE BUT SHOULD FIGURE OUT HOW TO ADD**
#           Player Statistics (Goals/Assists/Shots/Blocks, by EV/PP/SH) game logs


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
library(dlookr)


# CURRENT DATASET = 9,385 OBSERVATIONS ***

#Read in training dataset
nhl_training_data_final <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Model Training Dataset/full_training_dataset.csv",
                                    stringsAsFactors = FALSE
)

# Add columns from EDA below
nhl_training_data_final["PercDrafted_Cash_log"] <- lapply(nhl_training_data_final["PercDrafted_Cash"], log)

nhl_training_data_final["PercDrafted_GPP_log"] <- lapply(nhl_training_data_final["PercDrafted_GPP"], log)

nhl_training_data_final["DKPts_log"] <- lapply(nhl_training_data_final["DKPts"], log)

nhl_training_data_final$DKPts_log[nhl_training_data_final$DKPts_log==-Inf] <- 0

nhl_training_data_final["Salary_log"] <- lapply(nhl_training_data_final["Salary"], log)



# General description of the dataset
describe(nhl_training_data_final)

# Sorting by skewness
nhl_training_data_final %>%
  describe() %>%
  select(variable, skewness, mean, p25, p50, p75) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))

# perform normalization and visualization of numerical data
nhl_training_data_final[c('PercDrafted_Cash',
                          'PercDrafted_GPP',
                          'DKPts',
                          'Salary',
                          'GamesInSlate',
                          'AvgPointsPerGame',
                          'Implied_goals',
                          'PercDrafted_Cash_log',
                          'PercDrafted_GPP_log',
                          'DKPts_log',
                          'Salary_log')] %>%
  normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

plot_normality(nhl_training_data_final[c('PercDrafted_Cash',
                                         'PercDrafted_GPP',
                                         'DKPts',
                                         'Salary',
                                         'GamesInSlate',
                                         'AvgPointsPerGame',
                                         'Implied_goals',
                                         'PercDrafted_Cash_log',
                                         'PercDrafted_GPP_log',
                                         'DKPts_log',
                                         'Salary_log')])

# Conclusion : need to normalize percdrafted_cash, percdrafted_gpp, DKPts, and Salary

# perform normalization and visualization of new data
nhl_training_data_final[c('PercDrafted_Cash_log',
                          'PercDrafted_GPP_log',
                          'DKPts_log',
                          'Salary_log')] %>%
  normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

plot_normality(nhl_training_data_final[c('PercDrafted_Cash_log',
                                         'PercDrafted_GPP_log',
                                         'DKPts_log',
                                         'Salary_log')])


# Correlate all numeric values
correlate(nhl_training_data_final[c('PercDrafted_Cash_log',
                                    'PercDrafted_GPP_log',
                                    'DKPts_log',
                                    'Salary_log',
                                    'GamesInSlate',
                                    'AvgPointsPerGame',
                                    'Implied_goals')])

plot_correlate(nhl_training_data_final[c('PercDrafted_Cash_log',
                                         'PercDrafted_GPP_log',
                                         'DKPts_log',
                                         'Salary_log',
                                         'GamesInSlate',
                                         'AvgPointsPerGame',
                                         'Implied_goals')])


target_by() defines the target variable and relate() describes the relationship with the variables of interest corresponding to the target variable.
plot.relate() visualizes the relationship to the variable of interest corresponding to the destination variable.
eda_report()

