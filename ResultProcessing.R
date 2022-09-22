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
  # results$Collimator_error <- ifelse (data2$description.of.the.error == "Collimator angle should have been 0", 1, 0)
  # results$PTV_error_4000 <- ifelse (data2$description.of.the.error == "PTV dose should have been 4000cGy", 1, 0)
  # results$PTV_error_4500 <- ifelse (data2$description.of.the.error == "PTV dose should have been 4500cGy", 1, 0)
  # results$PTV_error_6000 <- ifelse (data2$description.of.the.error == "PTV dose should have been 6000cGy", 1, 0)
  # results$PTV_error_9100 <- ifelse (data2$description.of.the.error == "PTV dose should have been 9100cGy", 1, 0)
  # results$PTV_error <- ifelse (results$PTV_error_4500 + results$PTV_error_9100 + results$PTV_error_4000 + results$PTV_error_6000 > 0, 1, 0)
  # results$Table_error_0 <- ifelse (data2$description.of.the.error == "table angle should have been 0", 1, 0)
  # results$Table_error_10 <- ifelse (data2$description.of.the.error == "table angle should have been 10", 1, 0)
  # results$Table_error <- ifelse (results$Table_error_0 + results$Table_error_10 > 0, 1, 0)
  # results$Bolus_error_custom <- ifelse (data2$description.of.the.error == "Bolus should have been *custom", 1, 0)
  # results$Bolus_error_none <- ifelse (data2$description.of.the.error == "Bolus should have been NONE", 1, 0)
  # results$Bolus_error <- ifelse (results$Bolus_error_custom + results$Bolus_error_none > 0, 1, 0)
  # #results$RadiationType_error_06 <- ifelse (data2$description.of.the.error == "Rx_Radiation_Type should have been x06", 1, 0)
  # #results$RadiationType_error_10 <- ifelse (data2$description.of.the.error == "Rx_Radiation_Type should have been x10", 1, 0)
  # #results$RadiationType_error <- ifelse (results$RadiationType_error_06 + results$RadiationType_error_10 > 0, 1, 0)
  # results$GantryAngle_error_170 <- ifelse (data2$description.of.the.error == "Gantry_Angle should have started at 170", 1, 0)
  # results$GantryAngle_error_168 <- ifelse (data2$description.of.the.error == "Gantry_Angle should have started at 168", 1, 0)
  # results$GantryAngle_error <- ifelse (results$GantryAngle_error_170 + results$GantryAngle_error_168 > 0, 1, 0)
  
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
# Same site training and testing
################################################################################

resultsMAASTRO <- read.csv(paste(source_folder, "Output_validation_Maastro_20220908145811.csv", sep = "\\"))
resultsUVM <- read.csv(paste(source_folder, "Output_validation_UVM_20220908141619.csv", sep = "\\"))
resultsUW <- read.csv(paste(source_folder, "Output_validation_UW_20220908141628.csv", sep = "\\"))

dataMaastro <- read.csv(paste(source_folder, "Hugin-BN-RT-QA-MAASTRO_test.csv", sep = "\\"), sep = ',')
dataUVM<- read.csv(paste(source_folder, "UVM_1821_binned_error_v2.csv", sep = "\\"), sep = ',')
dataUW <- read.csv(paste(source_folder, "UWSCCA_1821_binned_error_v2.csv", sep = "\\"), sep = ',')

pred_vs_obs_MAASTRO <- getResults(resultsMAASTRO, dataMaastro)
pred_vs_obs_UVM <- getResults(resultsUVM, dataUVM)
pred_vs_obs_UW <- getResults(resultsUW, dataUW)

rocMAASTRO <-roc(pred_vs_obs_MAASTRO$Errors, 1-pred_vs_obs_MAASTRO$Predicted)
rocUVM <- roc(pred_vs_obs_UVM$Errors, 1-pred_vs_obs_UVM$Predicted)
rocUW <-  roc(pred_vs_obs_UW$Errors, 1-pred_vs_obs_UW$Predicted)

aucMaastro <- round(auc(rocMAASTRO)*100,1)
aucUVM <- round(auc(rocUVM)*100,1)
aucUW <- round(auc(rocUW)*100,1)

plot(rocUW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocMAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocUVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucUW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("UVM (AUC:", aucUVM,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

aucs_for_vars_MAASTRO <- aucSingleVariable(pred_vs_obs_MAASTRO)
aucs_for_vars_UVM     <- aucSingleVariable(pred_vs_obs_UVM)
aucs_for_vars_UW      <- aucSingleVariable(pred_vs_obs_UW)

write.csv(aucs_for_vars_MAASTRO, file = "aucs_for_vars_MAASTRO.csv")
write.csv(aucs_for_vars_UVM,     file = "aucs_for_vars_UVM.csv")
write.csv(aucs_for_vars_UW,      file = "aucs_for_vars_UW.csv")

pred_vs_obs_MAASTRO_var = pred_vs_obs_MAASTRO[pred_vs_obs_MAASTRO$Variable == "MU_Per_cGy",]
rocMAASTRO_var <-roc(pred_vs_obs_MAASTRO_var$Errors, 1-pred_vs_obs_MAASTRO_var$Predicted)
plot(rocMAASTRO_var)
auc(rocMAASTRO_var)

################################################################################
# Same site training and testing - instancing all
################################################################################

resultsMAASTRO <- read.csv(paste(source_folder, "Output_validation_complete_Maastro_20220909141506.csv", sep = "\\"))
resultsUVM <- read.csv(paste(source_folder, "Output_validation_complete_UVM_20220909144117.csv", sep = "\\"))
resultsUW <- read.csv(paste(source_folder, "Output_validation_complete_UW_20220909145034.csv", sep = "\\"))

dataMaastro <- read.csv(paste(source_folder, "Hugin-BN-RT-QA-MAASTRO_test.csv", sep = "\\"), sep = ',')
dataUVM<- read.csv(paste(source_folder, "UVM_1821_binned_error_v2.csv", sep = "\\"), sep = ',')
dataUW <- read.csv(paste(source_folder, "UWSCCA_1821_binned_error_v2.csv", sep = "\\"), sep = ',')

pred_vs_obs_MAASTRO <- getResults(resultsMAASTRO, dataMaastro)
pred_vs_obs_UVM <- getResults(resultsUVM, dataUVM)
pred_vs_obs_UW <- getResults(resultsUW, dataUW)

rocMAASTRO <-roc(pred_vs_obs_MAASTRO$Errors, 1-pred_vs_obs_MAASTRO$Predicted)
rocUVM <- roc(pred_vs_obs_UVM$Errors, 1-pred_vs_obs_UVM$Predicted)
rocUW <-  roc(pred_vs_obs_UW$Errors, 1-pred_vs_obs_UW$Predicted)

aucMaastro <- round(auc(rocMAASTRO)*100,1)
aucUVM <- round(auc(rocUVM)*100,1)
aucUW <- round(auc(rocUW)*100,1)

plot(rocUW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocMAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocUVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucUW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("UVM (AUC:", aucUVM,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)


aucs_for_vars_MAASTRO <- aucSingleVariable(pred_vs_obs_MAASTRO)
aucs_for_vars_UVM     <- aucSingleVariable(pred_vs_obs_UVM)
aucs_for_vars_UW      <- aucSingleVariable(pred_vs_obs_UW)

write.csv(aucs_for_vars_MAASTRO, file = "aucs_for_vars_MAASTRO_complete.csv")
write.csv(aucs_for_vars_UVM,     file = "aucs_for_vars_UVM_complete.csv")
write.csv(aucs_for_vars_UW,      file = "aucs_for_vars_UW_complete.csv")

################################################################################
# Cross site training and testing
################################################################################

resultsMAASTRO_UVM <- read.csv(paste(source_folder, "Output_validation_Maastro_UVM_20220912142115.csv", sep = "\\"))
resultsMAASTRO_UW <- read.csv(paste(source_folder, "Output_validation_Maastro_UW_20220912142119.csv", sep = "\\"))
resultsUVM_MAASTRO <- read.csv(paste(source_folder, "Output_validation_UVM_Maastro_20220912142134.csv", sep = "\\"))
resultsUVM_UW <- read.csv(paste(source_folder, "Output_validation_UVM_UW_20220912142201.csv", sep = "\\"))
resultsUW_MAASTRO <- read.csv(paste(source_folder, "Output_validation_UW_Maastro_20220912142213.csv", sep = "\\"))
resultsUW_UVM <- read.csv(paste(source_folder, "Output_validation_UW_UVM_20220912142235.csv", sep = "\\"))

testing_dataMaastro <- read.csv(paste(source_folder, "Hugin-BN-RT-QA-MAASTRO_test.csv", sep = "\\"), sep = ',')
testing_dataUVM<- read.csv(paste(source_folder, "UVM_1821_binned_error_v2.csv", sep = "\\"), sep = ',')
testing_dataUW <- read.csv(paste(source_folder, "UWSCCA_1821_binned_error_v2.csv", sep = "\\"), sep = ',')

pred_vs_obs_MAASTRO_UVM <- getResults(resultsMAASTRO_UVM, testing_dataUVM)
pred_vs_obs_MAASTRO_UW  <- getResults(resultsMAASTRO_UW, testing_dataUW)
pred_vs_obs_UVM_MAASTRO <- getResults(resultsUVM_MAASTRO, testing_dataMaastro)
pred_vs_obs_UVM_UW      <- getResults(resultsUVM_UW, testing_dataUW)
pred_vs_obs_UW_MAASTRO  <- getResults(resultsUW_MAASTRO, testing_dataMaastro)
pred_vs_obs_UW_UVM      <- getResults(resultsUW_UVM, testing_dataUVM)

rocMAASTRO_UVM <-roc(pred_vs_obs_MAASTRO_UVM$Errors, 1-pred_vs_obs_MAASTRO_UVM$Predicted)
rocMAASTRO_UW <-roc(pred_vs_obs_MAASTRO_UW$Errors, 1-pred_vs_obs_MAASTRO_UW$Predicted)
rocUVM_MAASTRO <- roc(pred_vs_obs_UVM_MAASTRO$Errors, 1-pred_vs_obs_UVM_MAASTRO$Predicted)
rocUVM_UW <- roc(pred_vs_obs_UVM_UW$Errors, 1-pred_vs_obs_UVM_UW$Predicted)
rocUW_MAASTRO <-  roc(pred_vs_obs_UW_MAASTRO$Errors, 1-pred_vs_obs_UW_MAASTRO$Predicted)
rocUW_UVM <-  roc(pred_vs_obs_UW_UVM$Errors, 1-pred_vs_obs_UW_UVM$Predicted)

aucMAASTRO_UVM <- round(auc(rocMAASTRO_UVM)*100,1)
aucMAASTRO_UW <- round(auc(rocMAASTRO_UW)*100,1)
aucUVM_MAASTRO <- round(auc(rocUVM_MAASTRO)*100,1)
aucUVM_UW <- round(auc(rocUVM_UW)*100,1)
aucUW_MAASTRO <- round(auc(rocUW_MAASTRO)*100,1)
aucUW_UVM <- round(auc(rocUW_UVM)*100,1)

# MAASTRO training
plot(rocMAASTRO_UW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocMAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocMAASTRO_UVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucMAASTRO_UW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("UVM (AUC:", aucMAASTRO_UVM,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

# UVM training
plot(rocUVM_UW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocUVM_MAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocUVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucUW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("UVM (AUC:", aucUVM,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

# UW training
plot(rocUW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocUW_MAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocUW_UVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucUW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("UVM (AUC:", aucUVM,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

aucs_for_vars_MAASTRO_UVM <- aucSingleVariable(pred_vs_obs_MAASTRO_UVM)
aucs_for_vars_MAASTRO_UW  <- aucSingleVariable(pred_vs_obs_MAASTRO_UW)
aucs_for_vars_UVM_MAASTRO <- aucSingleVariable(pred_vs_obs_UVM_MAASTRO)
aucs_for_vars_UVM_UW      <- aucSingleVariable(pred_vs_obs_UVM_UW)
aucs_for_vars_UW_MAASTRO  <- aucSingleVariable(pred_vs_obs_UW_MAASTRO)
aucs_for_vars_UW_UVM      <- aucSingleVariable(pred_vs_obs_UW_UVM)

write.csv(aucs_for_vars_MAASTRO_UVM,  file = paste0(source_folder, "\\", "aucs_for_vars_MAASTRO_UVM", ".csv"))
write.csv(aucs_for_vars_MAASTRO_UW,  file = paste0(source_folder, "\\","aucs_for_vars_MAASTRO_UW", ".csv"))
write.csv(aucs_for_vars_UVM_MAASTRO,  file = paste0(source_folder, "\\","aucs_for_vars_UVM_MAASTRO", ".csv"))
write.csv(aucs_for_vars_UVM_UW,  file = paste0(source_folder, "\\", "aucs_for_vars_UVM_UW", ".csv"))
write.csv(aucs_for_vars_UW_MAASTRO,  file = paste0(source_folder, "\\", "aucs_for_vars_UW_MAASTRO", ".csv"))
write.csv(aucs_for_vars_UW_UVM,  file = paste0(source_folder, "\\","aucs_for_vars_UW_UVM", ".csv"))

################################################################################
# Cross site training and testing - Complete
################################################################################

resultsMAASTRO_UVM <- read.csv(paste(source_folder, "Output_validation_complete_Maastro_UVM_20220909163513.csv", sep = "\\"))
resultsMAASTRO_UW <- read.csv(paste(source_folder, "Output_validation_complete_Maastro_UW_20220909180625.csv", sep = "\\"))
resultsUVM_MAASTRO <- read.csv(paste(source_folder, "Output_validation_complete_UVM_Maastro_20220909181155.csv", sep = "\\"))
resultsUVM_UW <- read.csv(paste(source_folder, "Output_validation_complete_UVM_UW_20220909181847.csv", sep = "\\"))
resultsUW_MAASTRO <- read.csv(paste(source_folder, "Output_validation_complete_UW_Maastro_20220909182439.csv", sep = "\\"))
resultsUW_UVM <- read.csv(paste(source_folder, "Output_validation_complete_UW_UVM_20220909183106.csv", sep = "\\"))

testing_dataMaastro <- read.csv(paste(source_folder, "Hugin-BN-RT-QA-MAASTRO_test.csv", sep = "\\"), sep = ',')
testing_dataUVM<- read.csv(paste(source_folder, "UVM_1821_binned_error_v2.csv", sep = "\\"), sep = ',')
testing_dataUW <- read.csv(paste(source_folder, "UWSCCA_1821_binned_error_v2.csv", sep = "\\"), sep = ',')

pred_vs_obs_MAASTRO_UVM <- getResults(resultsMAASTRO_UVM, testing_dataUVM)
pred_vs_obs_MAASTRO_UW  <- getResults(resultsMAASTRO_UW, testing_dataUW)
pred_vs_obs_UVM_MAASTRO <- getResults(resultsUVM_MAASTRO, testing_dataMaastro)
pred_vs_obs_UVM_UW      <- getResults(resultsUVM_UW, testing_dataUW)
pred_vs_obs_UW_MAASTRO  <- getResults(resultsUW_MAASTRO, testing_dataMaastro)
pred_vs_obs_UW_UVM      <- getResults(resultsUW_UVM, testing_dataUVM)

rocMAASTRO_UVM <-roc(pred_vs_obs_MAASTRO_UVM$Errors, 1-pred_vs_obs_MAASTRO_UVM$Predicted)
rocMAASTRO_UW <-roc(pred_vs_obs_MAASTRO_UW$Errors, 1-pred_vs_obs_MAASTRO_UW$Predicted)
rocUVM_MAASTRO <- roc(pred_vs_obs_UVM_MAASTRO$Errors, 1-pred_vs_obs_UVM_MAASTRO$Predicted)
rocUVM_UW <- roc(pred_vs_obs_UVM_UW$Errors, 1-pred_vs_obs_UVM_UW$Predicted)
rocUW_MAASTRO <-  roc(pred_vs_obs_UW_MAASTRO$Errors, 1-pred_vs_obs_UW_MAASTRO$Predicted)
rocUW_UVM <-  roc(pred_vs_obs_UW_UVM$Errors, 1-pred_vs_obs_UW_UVM$Predicted)

aucMAASTRO_UVM <- round(auc(rocMAASTRO_UVM)*100,1)
aucMAASTRO_UW <- round(auc(rocMAASTRO_UW)*100,1)
aucUVM_MAASTRO <- round(auc(rocUVM_MAASTRO)*100,1)
aucUVM_UW <- round(auc(rocUVM_UW)*100,1)
aucUW_MAASTRO <- round(auc(rocUW_MAASTRO)*100,1)
aucUW_UVM <- round(auc(rocUW_UVM)*100,1)

# MAASTRO training
plot(rocMAASTRO_UW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocMAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocMAASTRO_UVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucMAASTRO_UW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("UVM (AUC:", aucMAASTRO_UVM,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

# UVM training
plot(rocUVM_UW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocUVM_MAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocUVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucUW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("UVM (AUC:", aucUVM,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

# UW training
plot(rocUW, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocUW_MAASTRO, col = 4, lty = 3, add = TRUE)
plot(rocUW_UVM, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("UW (AUC:",aucUW ,"%)"), paste0("Maastro (AUC:", aucMaastro,"%)"), paste0("UVM (AUC:", aucUVM,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

aucs_for_vars_MAASTRO_UVM <- aucSingleVariable(pred_vs_obs_MAASTRO_UVM)
aucs_for_vars_MAASTRO_UW  <- aucSingleVariable(pred_vs_obs_MAASTRO_UW)
aucs_for_vars_UVM_MAASTRO <- aucSingleVariable(pred_vs_obs_UVM_MAASTRO)
aucs_for_vars_UVM_UW      <- aucSingleVariable(pred_vs_obs_UVM_UW)
aucs_for_vars_UW_MAASTRO  <- aucSingleVariable(pred_vs_obs_UW_MAASTRO)
aucs_for_vars_UW_UVM      <- aucSingleVariable(pred_vs_obs_UW_UVM)

write.csv(aucs_for_vars_MAASTRO_UVM,  file = paste0("aucs_for_vars_MAASTRO_UVM", "_complete", ".csv"))
write.csv(aucs_for_vars_MAASTRO_UW,  file = paste0("aucs_for_vars_MAASTRO_UW", "_complete", ".csv"))
write.csv(aucs_for_vars_UVM_MAASTRO,  file = paste0("aucs_for_vars_UVM_MAASTRO", "_complete", ".csv"))
write.csv(aucs_for_vars_UVM_UW,  file = paste0("aucs_for_vars_UVM_UW",  "_complete",".csv"))
write.csv(aucs_for_vars_UW_MAASTRO,  file = paste0("aucs_for_vars_UW_MAASTRO", "_complete", ".csv"))
write.csv(aucs_for_vars_UW_UVM,  file = paste0("aucs_for_vars_UW_UVM", "_complete", ".csv"))


###########################################
# Combine training data
###########################################

resultsMAASTRO_UW_train <- read.csv(paste(source_folder, "Output_validation_Maastro_UW_UVM_20220912151344.csv", sep = "\\"))
resultsMAASTRO_UVM_train <- read.csv(paste(source_folder, "Output_validation_Maastro_UVM_UW_20220912151348.csv", sep = "\\"))
resultsUVM_UW_train <- read.csv(paste(source_folder, "Output_validation_UVM_UW_Maastro_20220912151358.csv", sep = "\\"))

pred_vs_obs_MAASTRO_UW_train <- getResults(resultsMAASTRO_UW_train, testing_dataUVM)
pred_vs_obs_MAASTRO_UVM_train <- getResults(resultsMAASTRO_UVM_train, testing_dataUW)
pred_vs_obs_UVM_UW_train <- getResults(resultsUVM_UW_train, testing_dataMaastro)

rocMAASTRO_UW_train <-roc(pred_vs_obs_MAASTRO_UW_train$Errors, 1-pred_vs_obs_MAASTRO_UW_train$Predicted)
rocMAASTRO_UVM_train <- roc(pred_vs_obs_MAASTRO_UVM_train$Errors, 1-pred_vs_obs_MAASTRO_UVM_train$Predicted)
rocUVM_UW_train <-  roc(pred_vs_obs_UVM_UW_train$Errors, 1-pred_vs_obs_UVM_UW_train$Predicted)

aucMAASTRO_UW_train <- round(auc(rocMAASTRO_UW_train)*100,1)
aucMAASTRO_UVM_train <- round(auc(rocMAASTRO_UVM_train)*100,1)
aucUVM_UW_train <- round(auc(rocUVM_UW_train)*100,1)

plot(rocMAASTRO_UW_train, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocMAASTRO_UVM_train, col = 4, lty = 3, add = TRUE)
plot(rocUVM_UW_train, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("Maastro + UW (AUC:",aucMAASTRO_UW_train ,"%)"), paste0("Maastro + UVM (AUC:", aucMAASTRO_UVM_train,"%)"), paste0("UVM + UW (AUC:", aucUVM_UW_train,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

aucs_Excluding_vars_MAASTRO_UW_train <- aucExcludingVariable(pred_vs_obs_MAASTRO_UW_train)
aucs_Excluding_vars_MAASTRO_UVM_train <- aucExcludingVariable(pred_vs_obs_MAASTRO_UVM_train)
aucs_Excluding_vars_UVM_UW_train <- aucExcludingVariable(pred_vs_obs_UVM_UW_train)

write.csv(aucs_Excluding_vars_MAASTRO_UW_train, file = "aucs_Excluding_vars_MAASTRO_UW_train.csv")
write.csv(aucs_Excluding_vars_MAASTRO_UVM_train, file = "aucs_Excluding_vars_MAASTRO_UVM_train.csv")
write.csv(aucs_Excluding_vars_UVM_UW_train, file = "aucs_Excluding_vars_UVM_UW_train.csv")

aucs_for_vars_MAASTRO_UW_train  <- aucSingleVariable(pred_vs_obs_MAASTRO_UW_train)
aucs_for_vars_MAASTRO_UVM_train <- aucSingleVariable(pred_vs_obs_MAASTRO_UVM_train)
aucs_for_vars_UVM_UW_train      <- aucSingleVariable(pred_vs_obs_UVM_UW_train)

write.csv(aucs_for_vars_MAASTRO_UW_train,  file = "aucs_for_vars_MAASTRO_UW_train.csv")
write.csv(aucs_for_vars_MAASTRO_UVM_train, file = "aucs_for_vars_MAASTRO_UVM_train.csv")
write.csv(aucs_for_vars_UVM_UW_train,      file = "aucs_for_vars_UVM_UW_train.csv")

###########################################
# Combine training data - Complete info
###########################################

resultsMAASTRO_UW_train <- read.csv(paste(source_folder, "Output_validation_complete_Maastro_UW_UVM_20220912152005.csv", sep = "\\"))
resultsMAASTRO_UVM_train <- read.csv(paste(source_folder, "Output_validation_complete_Maastro_UVM_UW_20220912152218.csv", sep = "\\"))
resultsUVM_UW_train <- read.csv(paste(source_folder, "Output_validation_complete_UVM_UW_Maastro_20220912152726.csv", sep = "\\"))

pred_vs_obs_MAASTRO_UW_train <- getResults(resultsMAASTRO_UW_train, testing_dataUVM)
pred_vs_obs_MAASTRO_UVM_train <- getResults(resultsMAASTRO_UVM_train, testing_dataUW)
pred_vs_obs_UVM_UW_train <- getResults(resultsUVM_UW_train, testing_dataMaastro)

rocMAASTRO_UW_train <-roc(pred_vs_obs_MAASTRO_UW_train$Errors, 1-pred_vs_obs_MAASTRO_UW_train$Predicted)
rocMAASTRO_UVM_train <- roc(pred_vs_obs_MAASTRO_UVM_train$Errors, 1-pred_vs_obs_MAASTRO_UVM_train$Predicted)
rocUVM_UW_train <-  roc(pred_vs_obs_UVM_UW_train$Errors, 1-pred_vs_obs_UVM_UW_train$Predicted)

aucMAASTRO_UW_train <- round(auc(rocMAASTRO_UW_train)*100,1)
aucMAASTRO_UVM_train <- round(auc(rocMAASTRO_UVM_train)*100,1)
aucUVM_UW_train <- round(auc(rocUVM_UW_train)*100,1)

plot(rocMAASTRO_UW_train, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocMAASTRO_UVM_train, col = 4, lty = 3, add = TRUE)
plot(rocUVM_UW_train, col = 6, lty = 4, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("Maastro + UW (AUC:",aucMAASTRO_UW_train ,"%)"), paste0("Maastro + UVM (AUC:", aucMAASTRO_UVM_train,"%)"), paste0("UVM + UW (AUC:", aucUVM_UW_train,"%)")),
       col=c("red", "blue", "purple"), lty=c(2,3,4), cex=0.8)

aucs_Excluding_vars_MAASTRO_UW_train <- aucExcludingVariable(pred_vs_obs_MAASTRO_UW_train)
aucs_Excluding_vars_MAASTRO_UVM_train <- aucExcludingVariable(pred_vs_obs_MAASTRO_UVM_train)
aucs_Excluding_vars_UVM_UW_train <- aucExcludingVariable(pred_vs_obs_UVM_UW_train)

write.csv(aucs_Excluding_vars_MAASTRO_UW_train, file = "aucs_Excluding_vars_MAASTRO_UW_train_complete.csv")
write.csv(aucs_Excluding_vars_MAASTRO_UVM_train, file = "aucs_Excluding_vars_MAASTRO_UVM_train_complete.csv")
write.csv(aucs_Excluding_vars_UVM_UW_train, file = "aucs_Excluding_vars_UVM_UW_train_complete.csv")

aucs_for_vars_MAASTRO_UW_train  <- aucSingleVariable(pred_vs_obs_MAASTRO_UW_train)
aucs_for_vars_MAASTRO_UVM_train <- aucSingleVariable(pred_vs_obs_MAASTRO_UVM_train)
aucs_for_vars_UVM_UW_train      <- aucSingleVariable(pred_vs_obs_UVM_UW_train)

write.csv(aucs_for_vars_MAASTRO_UW_train,  file = "aucs_for_vars_MAASTRO_UW_train_complete.csv")
write.csv(aucs_for_vars_MAASTRO_UVM_train, file = "aucs_for_vars_MAASTRO_UVM_train_complete.csv")
write.csv(aucs_for_vars_UVM_UW_train,      file = "aucs_for_vars_UVM_UW_train_complete.csv")

###########################################
# Combine all training and testing data 
###########################################
testing_dataAll <- read.csv(paste(source_folder, "BN-RT-QA-all-test.csv", sep = "\\"))
resultsAll <- read.csv(paste(source_folder, "Output_validation_All_All_20220919174823.csv", sep = "\\"))
resultsAll_complete <- read.csv(paste(source_folder, "Output_validation_complete_All_All_20220919161837.csv", sep = "\\"))

pred_vs_obs_All <- getResults(resultsAll, testing_dataAll)
pred_vs_obs_All_complete <- getResults(resultsAll_complete, testing_dataAll)

rocAll <-roc(pred_vs_obs_All$Errors, 1-pred_vs_obs_All$Predicted)
rocAll_complete <- roc(pred_vs_obs_All_complete$Errors, 1-pred_vs_obs_All_complete$Predicted)

aucAll <- round(auc(rocAll)*100,1)
aucAll_complete <- round(auc(rocAll_complete)*100,1)

plot(rocAll, col = 2, lty = 2, main = "ROC", xlim=c(1,0), ylim=c(0,1))
plot(rocAll_complete, col = 4, lty = 3, add = TRUE)

legend(0.5, 0.2, legend=c(paste0("All (AUC:",aucAll,"%)"), paste0("All - complete info (AUC:", aucAll_complete,"%)")),
       col=c("red", "blue"), lty=c(2,3), cex=0.8)

aucs_for_vars_All          <- aucSingleVariable(pred_vs_obs_All)
aucs_for_vars_All_Complete <- aucSingleVariable(pred_vs_obs_All_complete)

write.csv(aucs_for_vars_All,  file = paste0("aucs_for_vars_All", ".csv"))
write.csv(aucs_for_vars_All_Complete,  file = paste0("aucs_for_vars_All", "_complete", ".csv"))

################################################################################

write.csv(pred_vs_obs_MAASTRO, paste(source_folder, "pred_vs_obs_MAASTRO.csv", sep="\\"))

################################################################################
# Old script
################################################################################
table(data2$description.of.the.error)

all_results_UW <- getResults(resultsUW, data)
all_results_MAASTRO <- getResults(resultsMAASTRO, data)
all_results_MAASTRO_UW <- getResults(resultsMAASTRO_UW, data)

thresholds <- seq(0,1,by=0.01)

sensitivities <- rep(0,length(thresholds))
specificities <- rep(0,length(thresholds))
i <- 1

lvs <- c("abnormal", "normal")
errors <- factor(ifelse (all_results$Errors == 0, "normal", "abnormal"), lvs)
for(t in thresholds){
  flag <- factor(ifelse(all_results$Predicted <= t, "abnormal", "normal"), lvs)
  sensitivities[i] <- caret::sensitivity(flag, errors)
  specificities[i] <- caret::specificity(flag, errors)
  i <- i+ 1
}


flag <- factor(ifelse(all_results$Predicted <= 0.535, "abnormal", "normal"), lvs)
caret::sensitivity(flag, errors)
caret::specificity(flag, errors)
caret::posPredValue(flag, errors, prevalence = 0.001)

#UW
errors <- factor(ifelse (all_results_UW$Errors == 0, "normal", "abnormal"), lvs)
flag <- factor(ifelse(all_results_UW$Predicted <= 0.522, "abnormal", "normal"), lvs)
caret::sensitivity(flag, errors)
caret::specificity(flag, errors)
caret::posPredValue(flag, errors, prevalence = 0.03)
sum(flag == "abnormal")/length(flag)

#Maastro
errors <- factor(ifelse (all_results_MAASTRO$Errors == 0, "normal", "abnormal"), lvs)
flag <- factor(ifelse(all_results_MAASTRO$Predicted <= 0.766, "abnormal", "normal"), lvs)
caret::sensitivity(flag, errors)
caret::specificity(flag, errors)
caret::posPredValue(flag, errors, prevalence = 0.03)
sum(flag == "abnormal")/length(flag)

#UW+Maastro
errors <- factor(ifelse (all_results_MAASTRO_UW$Errors == 0, "normal", "abnormal"), lvs)
flag <- factor(ifelse(all_results_MAASTRO_UW$Predicted <= 0.521, "abnormal", "normal"), lvs)
caret::sensitivity(flag, errors)
caret::specificity(flag, errors)
caret::posPredValue(flag, errors, prevalence = 0.03)
sum(flag == "abnormal")/length(flag)

# Create Line Chart

# convert factor to numeric for convenience


# set up the plot
colors <- rainbow(3)
plot(c(0,1), c(0,1), type="n", xlab="Threshold",
     ylab="Sens/spec" )

lines(thresholds, sensitivities, type="b", lwd=1.5,
      lty=1, col=colors[1], pch=1)
lines(thresholds, specificities, type="b", lwd=1.5,
      lty=3, col=colors[3], pch=2)

# add a title and subtitle

# add a legend
legend(0.08, 0.15, c("Sensitivity", "Specificity"), cex=0.8, col=c(colors[1],colors[3]),
       pch=c(18,19), lty=c(1,2), title="Legend")

count_zero <- apply(results_no_ie[,3:21], 2, FUN=function(x) sum ( x==0 ))

plt <-roc_plot1(all_results_UW$Errors, 1-all_results_UW$Predicted, force_bootstrap = TRUE, resamps = 2000)
plt <-roc_plot1(all_results_MAASTRO$Errors, 1-all_results_MAASTRO$Predicted, force_bootstrap = TRUE, resamps = 2000)
plt <-roc_plot1(all_results_MAASTRO_UW$Errors, 1-all_results_MAASTRO_UW$Predicted, force_bootstrap = TRUE, resamps = 2000)



#roc_plot1(results_no_ie$rt_error, min_prob, force_bootstrap = TRUE)

roc_plot1(results_no_ie$Collimator_error, 1-results_no_ie$Collimator_Angle, force_bootstrap = TRUE, plt = plt)

roc_plot1(results_no_ie$PTV_error,  1-results_no_ie$PTV_dose_Rx, force_bootstrap = TRUE)

roc_plot1(results_no_ie$Table_error,  1-results_no_ie$Table_Angle, force_bootstrap = TRUE)

roc_plot1(results_no_ie$Bolus_error,  1-results_no_ie$Bolus, force_bootstrap = TRUE)
