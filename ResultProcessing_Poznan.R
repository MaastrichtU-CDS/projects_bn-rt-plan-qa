library(classifierplots)
library(caret)
library(ggplot2)
library(pROC)
library(tidyverse)

source("MyClassifierPlots.R")

source_folder <- "C:\\Users\\i.bermejo\\Documents\\Data\\BN in RT"


getResults<- function (results, data)
{
  #errors_exclude <- ifelse(startsWith(data$errors_description, "same"),1,0)
  #errors_exclude <- ifelse(startsWith(data$errors_description, "Same"),1,errors_exclude)
  errors_exclude <- ifelse(startsWith(data$errors_description, "Treatment"),1,0)
  errors_exclude <- ifelse(startsWith(data$errors_description, "treatment"),1,errors_exclude)
  data <- data[errors_exclude == 0,]
  results <- results[errors_exclude == 0,]
  
  #errors
  # Put the results for all variables in 2 columns: predicted and observed
  errors = results[,1:20]
  errors[,2:20] = 0
  errors$Beam_Energy = ifelse(startsWith(data$errors_description, "Beam energy"), 1, errors$Beam_Energy)
  errors$Beam_Energy = ifelse(startsWith(data$errors_description, "beamenergy"), 1, errors$Beam_Energy)
  errors$Bolus = ifelse(startsWith(data$errors_description, "Bolus"), 1, errors$Bolus)
  errors$Bolus = ifelse(startsWith(data$errors_description, "bolus"), 1, errors$Bolus)
  errors$Collimator_Angle = ifelse(startsWith(data$errors_description, "Collimator Angle"), 1, errors$Collimator_Angle)
  errors$Collimator_Angle = ifelse(startsWith(data$errors_description, "collimatorangle"), 1, errors$Collimator_Angle)
  errors$Gantry_Angle = ifelse(startsWith(data$errors_description, "Gantry Angle"), 1, errors$Gantry_Angle)
  errors$Gantry_Angle = ifelse(startsWith(data$errors_description, "gantryangle"), 1, errors$Gantry_Angle)
  errors$MU_Per_cGy = ifelse(startsWith(data$errors_description, "MU_Per_cGy"), 1, errors$MU_Per_cGy)
  errors$MU_Per_cGy = ifelse(startsWith(data$errors_description, "mu_per_cgy"), 1, errors$MU_Per_cGy)
  errors$MU_Per_Deg = ifelse(startsWith(data$errors_description, "MU_Per_Deg"), 1, errors$MU_Per_Deg)
  errors$MU_Per_Deg = ifelse(startsWith(data$errors_description, "mu_per_deg"), 1, errors$MU_Per_Deg)
  errors$Number_of_Beams = ifelse(startsWith(data$errors_description, "Number_of_beams"), 1, errors$Number_of_Beams)
  errors$Number_of_Beams = ifelse(startsWith(data$errors_description, "number_of_beams"), 1, errors$Number_of_Beams)
  errors$Radiation_Type = ifelse(startsWith(data$errors_description, "Rx_Radiation_Type"), 1, errors$Radiation_Type)
  errors$Radiation_Type = ifelse(startsWith(data$errors_description, "radiation_type"), 1, errors$Radiation_Type)
  errors$SSD = ifelse(startsWith(data$errors_description, "SSD"), 1, errors$SSD)
  errors$SSD = ifelse(startsWith(data$errors_description, "ssd"), 1, errors$SSD)
  errors$Table_Angle = ifelse(startsWith(data$errors_description, "Table Angle"), 1, errors$Table_Angle)
  errors$Table_Angle = ifelse(startsWith(data$errors_description, "tableangle"), 1, errors$Table_Angle)
  errors$Number_of_Fractions = ifelse(grepl("Number of fractions should be", data$errors_description), 1, errors$Number_of_Fractions)
  errors$Number_of_Fractions = ifelse(grepl("numberoffractionsshouldbe", data$errors_description), 1, errors$Number_of_Fractions)
  errors$Dose_Per_Fraction = ifelse(grepl("dose_per_fractionshouldbe", data$errors_description), 1, errors$Dose_Per_Fraction)
  errors$Dose_Per_Fraction = ifelse(grepl("Dose_Per_Fraction should be", data$errors_description), 1, errors$Dose_Per_Fraction)
  errors$PTV_Dose_Rx = ifelse(grepl("PTV_Dose_Rx should be", data$errors_description), 1, errors$PTV_Dose_Rx)
  errors$PTV_Dose_Rx = ifelse(grepl("ptv_dose_rxshouldbe", data$errors_description), 1, errors$PTV_Dose_Rx)
  errors$Description <- data$errors_description
  
  probabilities  = pivot_longer(results, cols = 2:20, names_to = "Variable", values_to = "prob")
  errors = pivot_longer(errors, cols = 2:20, names_to = "Variable", values_to = "error")
  
  all_results <- data.frame(Id = probabilities$Researchnumber, Variable = probabilities$Variable, Predicted = probabilities$prob, Errors = errors$error, Description = errors$Description)
  all_results <- na.omit(all_results)
  return(all_results)
}

aucExcludingVariable <- function(pred_vs_obs_exceptVar_full)
{
  test_variables <- unique(pred_vs_obs_exceptVar_full$Variable)
  aucs_exceptVar = data.frame(Variable = test_variables, Auc = rep(0, length(test_variables)))
  
  i = 1
  for(variable in test_variables)
  {
    pred_vs_obs_exceptVar <- pred_vs_obs_exceptVar_full[pred_vs_obs_exceptVar_full$Variable != variable,]
    roc_exceptVar <- roc(pred_vs_obs_exceptVar$Errors, 1-pred_vs_obs_exceptVar$Predicted)
    aucs_exceptVar$Auc[i] <- round(auc(roc_exceptVar)*100,1)
    i = i + 1
  }
  
  return(aucs_exceptVar)
}

aucSingleVariable <- function(pred_vs_obs_exceptVar_full)
{
  test_variables <- unique(pred_vs_obs_exceptVar_full$Variable[pred_vs_obs_exceptVar_full$Errors == 1])
  test_variables <- test_variables[order(test_variables)]
  aucs_exceptVar = data.frame(Variable = test_variables, Auc = rep(0, length(test_variables)))
  
  i = 1
  for(variable in test_variables)
  {
    pred_vs_obs_exceptVar <- pred_vs_obs_exceptVar_full[pred_vs_obs_exceptVar_full$Variable == variable,]
    roc_exceptVar <- roc(pred_vs_obs_exceptVar$Errors, 1-pred_vs_obs_exceptVar$Predicted)
    aucs_exceptVar$Auc[i] <- round(auc(roc_exceptVar)*100,1)
    i = i + 1
  }
  
  return(aucs_exceptVar)
}


################################################################################
# Poznan testing
################################################################################

resultsMAASTRO_Poznan <- read.csv("Output_validation_Maastro_Poznan_20220912142115.csv")
resultsUW_Poznan <- read.csv( "Output_validation_UW_Maastro_20220912142213.csv")
resultsAll_Poznan <- read.csv("Output_validation_UW_UVM_20220912142235.csv")
dataPoznan <- read.csv(paste(source_folder, "", sep = "\\"), sep = ',')

pred_vs_obs_MAASTRO <- getResults(resultsMAASTRO_Poznan, dataPoznan)
pred_vs_obs_UW <- getResults(resultsUW_Poznan, dataPoznan)
pred_vs_obs_All <- getResults(resultsAll_Poznan, dataPoznan)

rocMAASTRO <-roc(pred_vs_obs_MAASTRO$Errors, 1-pred_vs_obs_MAASTRO$Predicted)
rocUW <-  roc(pred_vs_obs_UW$Errors, 1-pred_vs_obs_UW$Predicted)
rocUW <-  roc(pred_vs_obs_All$Errors, 1-pred_vs_obs_All$Predicted)

aucMaastro <- round(auc(rocMAASTRO)*100,1)
aucUW <- round(auc(rocUW)*100,1)
aucAll <- round(auc(rocAll)*100,1)

plot(rocUW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocMAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocUVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucUW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("All (AUC:", aucAll,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)


aucs_for_vars_MAASTRO <- aucSingleVariable(pred_vs_obs_MAASTRO)
aucs_for_vars_UW      <- aucSingleVariable(pred_vs_obs_UW)
aucs_for_vars_All      <- aucSingleVariable(pred_vs_obs_All)

write.csv(aucs_for_vars_MAASTRO, file = "aucs_for_vars_MAASTRO.csv")
write.csv(aucs_for_vars_UW,      file = "aucs_for_vars_UW.csv")
write.csv(aucs_for_vars_All,      file = "aucs_for_vars_All.csv")

################################################################################
# Poznan testing - instancing all
################################################################################

resultsMAASTRO_Poznan <- read.csv("Output_validation_complete_Maastro_Poznan_20220912142115.csv")
resultsUW_Poznan <- read.csv( "Output_validation_complete_UW_Poznan_20220912142213.csv")
resultsAll_Poznan <- read.csv("Output_validation_complete_UW_Poznan_20220912142235.csv")

dataPoznan <- read.csv(paste(source_folder, "", sep = "\\"), sep = ',')

pred_vs_obs_MAASTRO <- getResults(resultsMAASTRO_Poznan, dataPoznan)
pred_vs_obs_UW <- getResults(resultsUW_Poznan, dataPoznan)
pred_vs_obs_All <- getResults(resultsAll_Poznan, dataPoznan)

rocMAASTRO <-roc(pred_vs_obs_MAASTRO$Errors, 1-pred_vs_obs_MAASTRO$Predicted)
rocUW <-  roc(pred_vs_obs_UW$Errors, 1-pred_vs_obs_UW$Predicted)
rocUW <-  roc(pred_vs_obs_All$Errors, 1-pred_vs_obs_All$Predicted)

aucMaastro <- round(auc(rocMAASTRO)*100,1)
aucUW <- round(auc(rocUW)*100,1)
aucAll <- round(auc(rocAll)*100,1)

plot(rocUW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocMAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocUVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucUW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("All (AUC:", aucAll,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)


aucs_for_vars_MAASTRO <- aucSingleVariable(pred_vs_obs_MAASTRO)
aucs_for_vars_UW      <- aucSingleVariable(pred_vs_obs_UW)
aucs_for_vars_All      <- aucSingleVariable(pred_vs_obs_All)

write.csv(aucs_for_vars_MAASTRO, file = "aucs_for_vars_MAASTRO_complete.csv")
write.csv(aucs_for_vars_UW,      file = "aucs_for_vars_UW_complete.csv")
write.csv(aucs_for_vars_All,      file = "aucs_for_vars_All_complete.csv")
