# Model Testing
#Have:
#   All Contest Data (Player, Date, Ownership %, DK Pts) aggregated
#   All DK Salary Data (Player, Date, Position, Salary, Games in Slate) aggregated
#   Player Statistics (Player, Date, Team, Opponent, Goals/Assists?Shots/Blocks, by EV/PP/SH) game logs

#Final Format of Training Data for Ownership Prediction Model (First Pass):
#   Date
#   Player
#   Salary
#   Position (actual and DK?)
#   Line
#   PP_Line
#   Team_Implied_Goals
#   Home Flag
#   Impact_Player_Flag
#   Games_in_Slate

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


#SET WORKING DIRECTORY (I.E. FOLDER FOR CURRENT FILE PATH)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Read in training dataset
nhl_training_data_final <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Model Training Dataset/full_training_dataset.csv",
                                    stringsAsFactors = FALSE
)



#Going to implement using Caret for now; can change if necessary
# Ownership Model - Steps:
# 1.) Split dataset
# 2.) Determine if feature selection is necessary or not; if so, remove some columns
# 3.) Implement several different model approaches
# 4.) Pick the best one
# 5.) Evaluate accuracy
# 6.) If good, implement to use moving forward in nightly run
# 7.) Repeat for DK Pts
#   7a.) Could instead do something related to probability of reaching "value" if exact points does not go well

# 1.) Cash PercDrafted Model

#Create separate dataset with only variables that you need to scale
nhl_percdrafted_cash_scaled <- scale(nhl_training_data_final[, -c(1:2, 7:22)]) #"Date","Name","PercDrafted_Cash","PercDrafted_GPP","DKPts",9:20)])

#Merge back with columns from orig dataset
nhl_percdrafted_cash_data <- cbind(nhl_training_data_final[c(7,10:23)], as.data.frame(nhl_percdrafted_cash_scaled))




#Split dataset
index <- createDataPartition(nhl_percdrafted_cash_data$PercDrafted_Cash, p=0.75, list=FALSE)
nhl_cash_trainSet <- nhl_percdrafted_cash_data[ index,]
nhl_cash_testSet <- nhl_percdrafted_cash_data[-index,]

#Set outcome name and predictors
outcomeName <-'PercDrafted_Cash'

predictors <- names(nhl_cash_trainSet)[!names(nhl_cash_trainSet) %in% outcomeName]


#GONNA SKIP THE BELOW FOR NOW
# #Feature selection using rfe in caret
# control <- rfeControl(functions = rfFuncs,
#                       method = "repeatedcv",
#                       repeats = 3,
#                       verbose = FALSE)
# Loan_Pred_Profile <- rfe(nhl_cash_trainSet[,predictors], nhl_cash_trainSet[,outcomeName],
#                          rfeControl = control)
# Loan_Pred_Profile
# 

#Train Model(s)
model_gbm <- train(nhl_cash_trainSet[,predictors], nhl_cash_trainSet[,outcomeName], method='gbm')
# model_rf <- train(nhl_cash_trainSet[,predictors], nhl_cash_trainSet[,outcomeName], method='rf')
# model_nnet <- train(nhl_cash_trainSet[,predictors], nhl_cash_trainSet[,outcomeName], method='nnet')
# model_glm <- train(nhl_cash_trainSet[,predictors], nhl_cash_trainSet[,outcomeName], method='glm')

#For parameter tuning
#modelLookup(model='gbm')

# #Change training iterations
# fitControl <- trainControl(
#   method = "repeatedcv",
#   number = 5,
#   repeats = 5)
# 
# #Creating grid
# grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))
# 
# # training the model
# model_gbm <- train(trainSet[,predictors], trainSet[,outcomeName], method='gbm', trControl=fitControl, tuneGrid=grid)

# summarizing the model
print(model_gbm)
# print(model_rf)
# print(model_nnet)
# print(model_glm)

#Make Predictions
# predictions <- predict.train(object=model_gbm, nhl_cash_testSet[,predictors], type="raw")
# table(predictions)
# 
# confusionMatrix(predictions, nhl_cash_testSet[,outcomeName])
# 
# table(factor(predictions, levels=min(nhl_cash_testSet[,outcomeName]):max(nhl_cash_testSet[,outcomeName])), 
#       factor(nhl_cash_testSet[,outcomeName], levels=min(nhl_cash_testSet[,outcomeName]):max(nhl_cash_testSet[,outcomeName])))
# 
# pred_temp <- data.table(pred = predictions,
#                         actual = nhl_cash_testSet[,outcomeName])
# 
# pred_temp['diff'] <- pred_temp$pred - pred_temp$actual


#Save off model as RDS file
saveRDS(model_gbm, "../Historical Data and Projections/Model Data/PercDrafted_Cash_GBM_Model.rds")


# 2.) GPP PercDrafted Model


#Create separate dataset with only variables that you need to scale
nhl_percdrafted_gpp_scaled <- scale(nhl_training_data_final[, -c(1:2, 7:23)])

#Merge back with columns from orig dataset
nhl_percdrafted_gpp_data <- cbind(nhl_training_data_final[c(8,10:23)], as.data.frame(nhl_percdrafted_gpp_scaled))


#Split dataset
index <- createDataPartition(nhl_percdrafted_gpp_data$PercDrafted_GPP, p=0.75, list=FALSE)
nhl_gpp_trainSet <- nhl_percdrafted_gpp_data[ index,]
nhl_gpp_testSet <- nhl_percdrafted_gpp_data[-index,]

#Set outcome name and predictors
outcomeName <-'PercDrafted_GPP'

predictors <- names(nhl_gpp_trainSet)[!names(nhl_gpp_trainSet) %in% outcomeName]


#Train Model(s)
gpp_model_gbm <- train(nhl_gpp_trainSet[,predictors], nhl_gpp_trainSet[,outcomeName], method='gbm')
# summarizing the model
print(gpp_model_gbm)

#Make Predictions
predictions <- predict.train(object=gpp_model_gbm, nhl_gpp_testSet[,predictors], type="raw")
#table(predictions)
# 
#confusionMatrix(predictions, nhl_gpp_testSet[,outcomeName])
# 
# table(factor(predictions, levels=min(nhl_cash_testSet[,outcomeName]):max(nhl_cash_testSet[,outcomeName])), 
#       factor(nhl_cash_testSet[,outcomeName], levels=min(nhl_cash_testSet[,outcomeName]):max(nhl_cash_testSet[,outcomeName])))
# 
pred_temp <- data.frame(pred = predictions,
                        actual = nhl_gpp_testSet[,"PercDrafted_GPP"])
# 
pred_temp['diff'] <- pred_temp$pred - pred_temp$actual


#Save off model as RDS file
saveRDS(gpp_model_gbm, "../Historical Data and Projections/Model Data/PercDrafted_GPP_GBM_Model.rds")



# 3.) DK Points Model

#Create separate dataset with only variables that you need to scale
nhl_dkpts_scaled <- scale(nhl_training_data_final[, -c(1:2, 10:23)])

#Merge back with columns from orig dataset
nhl_dkpts_data <- cbind(nhl_training_data_final[c(9:23)], as.data.frame(nhl_percdrafted_gpp_scaled))



#Split dataset
index <- createDataPartition(nhl_dkpts_data$DKPts, p=0.75, list=FALSE)
nhl_dkpts_trainSet <- nhl_dkpts_data[ index,]
nhl_dkpts_testSet <- nhl_dkpts_data[-index,]

#Set outcome name and predictors
outcomeName <-'DKPts'

predictors <- names(nhl_dkpts_trainSet)[!names(nhl_dkpts_trainSet) %in% outcomeName]


#Train Model(s)
dkpts_model_gbm <- train(nhl_dkpts_trainSet[,predictors], nhl_dkpts_trainSet[,outcomeName], method='gbm')
# summarizing the model
print(dkpts_model_gbm)

#Make Predictions
# predictions <- predict.train(object=model_gbm, nhl_cash_testSet[,predictors], type="raw")
# table(predictions)
# 
# confusionMatrix(predictions, nhl_cash_testSet[,outcomeName])
# 
# table(factor(predictions, levels=min(nhl_cash_testSet[,outcomeName]):max(nhl_cash_testSet[,outcomeName])), 
#       factor(nhl_cash_testSet[,outcomeName], levels=min(nhl_cash_testSet[,outcomeName]):max(nhl_cash_testSet[,outcomeName])))
# 
# pred_temp <- data.table(pred = predictions,
#                         actual = nhl_cash_testSet[,outcomeName])
# 
# pred_temp['diff'] <- pred_temp$pred - pred_temp$actual


#Save off model as RDS file
saveRDS(dkpts_model_gbm, "../Historical Data and Projections/Model Data/PercDrafted_GPP_GBM_Model.rds")


