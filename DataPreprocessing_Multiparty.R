library(ggplot2)
library(treemap)
# install.packages("devtools")
# library(devtools)
# install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
library(readxl)
library(dplyr)
library(gRain)
library(bnlearn)

####################################################
# Functions
####################################################

check_binning <- function(binning, df)
{
  for(column in colnames(binning)){
    if(column %in% colnames(df))
    {
      missingInBinning <- setdiff(unique(df[[column]]), unique(na.omit(binning[[column]])));
      if(length(missingInBinning) > 0 )
        print(paste0("Variable ", column ,": The following states in the data are not in the binning: ", paste(missingInBinning)));
      #missingInData <- setdiff(unique(binning[[column]]), unique(df[[column]]));
      #if(length(missingInData) > 0 )
      #   print(paste0("Variable ", column ,": The following states in the binning are not in the data: ", paste(missingInData)));
      
    }else
    {
      print(paste0("variable ", column, " is not in the data."))
    }
  }
}

####################################################
# Global variables
####################################################

source_folder <- "C:\\Users\\i.bermejo\\Documents\\Data\\BN in RT"

sub_folder <- "tmp"

binning <- read_excel(paste(source_folder, "Binning-20220606.xlsx", sep = "\\"), sheet = 'Binning_after_20220603')

####################################################
# Read MAASTRO data
####################################################
site <- "Maastro"
#data <- read.csv(paste(source_folder, "updated_data_150422.csv", sep = "\\"), sep = ',')
data <- read_excel(paste(source_folder, "updated_data_290722.xlsx", sep = "\\"))

#data <- data %>% select(-c("Researchnumber"))

data <- data.frame(lapply(data, function(x) {
                  gsub(",", "", x)
              }))

#data$Dose_Per_Fraction <- gsub("less or equal than 150", "<=150", data$Dose_Per_Fraction)
#data$Dose_Per_Fraction <- gsub("more than 3000", ">3000", data$Dose_Per_Fraction)
#data$MU_cGy <- gsub("2.5-3", "2.6-3", data$MU_cGy)

#data <- data.frame(lapply(data, function(x) {
#  gsub("more than ", ">", x)
#}))

data <- data.frame(lapply(data, function(x) {
  gsub("\"", "", x)
}))


check_binning(binning, data)

#anatomic_tumour_loc_breakdown <- as.data.frame(table(data$Anatomic_tumor_location))

#table_tot_dose_tumor_loc_trt_int = as.data.frame(with(data, table(PTV_Dose_Rx, Anatomic_tumor_location, Treatment_Intent)))

#treemap(anatomic_tumour_loc_breakdown,
#        index="Var1",
#        vSize="Freq",
#        type="index"
#)

data_errors <- data[data$errors == 1,]
data <- data[data$errors == 0,]

# shuffle data
set.seed(42)
rows <- sample(nrow(data))
data <- data[rows, ]

#split into train and test
split <- round(nrow(data) * 0.8)

data_test <- data[(split+1):nrow(data),]
data_test <- rbind(data_test,data_errors)
data_train <- data[1:split,] 

write.csv(data_test, paste(source_folder,"Hugin-BN-RT-QA-MAASTRO_test.csv", sep = "\\"), row.names = FALSE)

data_train <- data_train[-c(1,2,3)]
data_test_assumed_correct <- data_test[c(5,6,7,8,17)]
data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
data_test_assumed_correct[data_test_assumed_correct == "\"NULL\""] <- NA

data_train_hugin <- replace(data_train, TRUE, lapply(data_train, shQuote, type = "cmd"))
data_train_hugin[data_train_hugin == "\"NULL\""] <- NA
write.csv(data_train_hugin, paste(source_folder,"Hugin-BN-RT-QA-MAASTRO_train.dat", sep = "\\"), row.names = FALSE,  na = "", quote = FALSE)
write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, "Hugin-BN-RT-QA-MAASTRO_test.dat", sep = "\\"), row.names = FALSE)


for(i in 1:length(colnames(data_test)))
{
  variable <- colnames(data_test)[i]
  print(variable)
  variables_to_exclude <- c(c(1,2,3),i)
  if(!(variable %in% c("Researchnumber", "errors", "errors_description", "Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")))
  {
    data_test_assumed_correct <- data_test[-variables_to_exclude]   
    data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
    data_test_assumed_correct[data_test_assumed_correct == "\"NULL\""] <- NA
    write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, sub_folder, paste0("Hugin-BN-RT-QA-MAASTRO_test_",variable,".dat"), sep = "\\"), row.names = FALSE)
  }
}

# Replace manually "NULL" by <EMPTY>

###########################################
# Read Vermont data
###########################################
site <- "UVM"
data_UVM_train <- read.csv(paste(source_folder, "UVM_1821_binned_training_v2.csv", sep = "\\"))
data_UVM_test <- read.csv(paste(source_folder, "UVM_1821_binned_error_v2.csv", sep = "\\"))
colnames(data_UVM)<- gsub("bins_", "", colnames(data_UVM))
check_binning(binning, data_UVM_train)
check_binning(binning, data_UVM_test)

colnames(data_UVM_test)

data_test_assumed_correct <- data_UVM_test[c("Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")]
data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
data_test_assumed_correct[data_test_assumed_correct == "\"\""] <- NA
write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, "Hugin-BN-RT-QA-UVM_test.dat", sep = "\\"), row.names = FALSE)

for(i in 1:length(colnames(data_UVM_test)))
{
  variable <- colnames(data_UVM_test)[i]
  print(variable)
  variables_to_exclude <- c(c(1,2,3,5),i)
  if(!(variable %in% c("Researchnumber", "MU", "errors", "errors_description", "Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")))
  {
    data_test_assumed_correct <- data_UVM_test[-variables_to_exclude]   
    data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
    data_test_assumed_correct[data_test_assumed_correct == "\"\""] <- NA
    write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, sub_folder, paste0("Hugin-BN-RT-QA-",site,"_test_",variable,".dat"), sep = "\\"), row.names = FALSE)
  }
}

###########################################
# Read UW data
###########################################
site <- "UW"
data_UW_train <- read.csv(paste(source_folder, "UWSCCA_1821_binned_training_v2.csv", sep = "\\"))
data_UW_test <- read.csv(paste(source_folder, "UWSCCA_1821_binned_error_v2.csv", sep = "\\"))
check_binning(binning, data_UW_train)
check_binning(binning, data_UW_test)

data_test_assumed_correct <- data_UW_test[c("Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")]
data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
data_test_assumed_correct[data_test_assumed_correct == "\"\""] <- NA
write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, sub_folder, "Hugin-BN-RT-QA-test.dat", sep = "\\"), row.names = FALSE)

for(i in 1:length(colnames(data_UW_test)))
{
  variable <- colnames(data_UW_test)[i]
  print(variable)
  variables_to_exclude <- c(c(1,2,3,5),i)
  if(!(variable %in% c("Researchnumber", "MU", "errors", "errors_description", "Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")))
  {
    data_test_assumed_correct <- data_UW_test[-variables_to_exclude]   
    data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
    data_test_assumed_correct[data_test_assumed_correct == "\"\""] <- NA
    write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, sub_folder, paste0("Hugin-BN-RT-QA-",site,"_test_",variable,".dat"), sep = "\\"), row.names = FALSE)
  }
}

###########################################
# Combine training data
###########################################

training_variables <- colnames(data_train)
data_train[data_train == "NULL"] <- NA
data_UW_train <- select(data_UW_train, all_of(training_variables))
data_UW_train[data_UW_train == ""] <- NA
data_UVM_train <- select(data_UVM_train, all_of(training_variables))
data_UVM_train[data_UVM_train == ""] <- NA

data_Maastro_UW_train <- rbind(data_train, data_UW_train)
data_Maastro_UVM_train <- rbind(data_train, data_UVM_train)
data_UVM_UW_train <- rbind(data_UVM_train, data_UW_train)
data_all_train <- rbind(data_Maastro_UW_train, data_Maastro_UVM_train)

write.csv(data_Maastro_UW_train, na = "<EMPTY>", paste(source_folder, "Hugin-BN-RT-QA-Maastro_UW_train.dat", sep = "\\"), row.names = FALSE)
write.csv(data_Maastro_UVM_train, na = "<EMPTY>", paste(source_folder, "Hugin-BN-RT-QA-Maastro_UVM_train.dat", sep = "\\"), row.names = FALSE)
write.csv(data_UVM_UW_train, na = "<EMPTY>", paste(source_folder, "Hugin-BN-RT-QA-UVM_UW_train.dat", sep = "\\"), row.names = FALSE)
write.csv(data_all_train, na = "<EMPTY>", paste(source_folder, "Hugin-BN-RT-QA-All_train.dat", sep = "\\"), row.names = FALSE)

###########################################
# Combine testing data
###########################################
data_all_test <- rbind(data_UW_test, data_UVM_test)[-c(3,5)] 
data_all_test <- rbind(data_test[-c(1)], data_all_test)
write.csv(data_all_test, na = "", quote = FALSE, paste(source_folder, "BN-RT-QA-All-test.csv", sep = "\\"), row.names = FALSE)
data_all_test <- data_all_test[-c(1,2)]
data_test_assumed_correct <- data_all_test[c("Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")]
data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
data_test_assumed_correct[data_test_assumed_correct == "\"\""] <- NA
data_test_assumed_correct[data_test_assumed_correct == "\"NULL\""] <- NA
write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, "Hugin-BN-RT-QA-all_test.dat", sep = "\\"), row.names = FALSE)

for(i in 1:length(colnames(data_all_test)))
{
  variable <- colnames(data_all_test)[i]
  print(variable)
  variables_to_exclude <- c(i)
  if(!(variable %in% c("Researchnumber", "MU", "errors", "errors_description", "Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")))
  {
    data_test_assumed_correct <- data_all_test[-variables_to_exclude] 
    data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
    data_test_assumed_correct[data_test_assumed_correct == "\"\""] <- NA
    data_test_assumed_correct[data_test_assumed_correct == "\"NULL\""] <- NA
    write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, sub_folder, paste0("Hugin-BN-RT-QA-all_test_",variable,".dat"), sep = "\\"), row.names = FALSE)
  }
}

###########################################
# Old script
###########################################

data[data$description.of.the.error=='PTV dose should have been 4500cGy',c('PTV_Dose_Rx', 'Anatomic_tumour_loc', 'Treatment_Intent')]

data$Dose_Per_Fraction_num = as.numeric(ifelse(data$Dose_Per_Fraction == 'less than 150', 100, data$Dose_Per_Fraction))

hist(data$Dose_Per_Fraction_num, breaks = 100, main = "Histogram of Dose per Fraction")

hist(data$Dose_Per_Fraction_num[data$Dose_Per_Fraction_num < 500], breaks = 100, main = "Histogram of Dose per Fraction [0, 500]")

hist(data$Dose_Per_Fraction_num[data$Dose_Per_Fraction_num < 1000], breaks = 100, main = "Histogram of Dose per Fraction [0, 1000]")

table(data$Dose_Per_Fraction_num)

ggplot2.histogram(data=data, xName='Dose_Per_Fraction_num', groupName='Treatment_Intent',
                  legendPosition="top",
                  faceting=TRUE, facetingVarNames=c("Treatment_Intent", "Anatomic_tumour_loc"))

data$PTV_Dose_Rx = as.numeric(data$PTV_Dose_Rx)

ggplot2.histogram(data=data, xName='PTV_Dose_Rx',
                  groupName='Anatomic_tumour_loc', legendPosition="top",
                  faceting=TRUE, facetingVarNames="Anatomic_tumour_loc",
                  facetingDirection="horizontal") 

# shuffle data
set.seed(42)
rows <- sample(nrow(data))
data <- data[rows, ]

#split into train and test
split <- round(nrow(data) * 0.8)
data_train <- data[1:split,] 
data_test <- data[(split+1):nrow(data),] 

table(data_test$description.of.the.error)

#remove errors from training set
data_train = data_train[data_train$rt_error==0, ]

#remove NULL values
data_test = data_train[data_train$Couch_Lat !="NULL", ]
data_train = data_train[data_train$Couch_Long !="NULL", ]
data_train = data_train[data_train$Couch_Vert !="NULL", ]

#save files in Hugin format
write.csv(subset(data_train, select = -c(Researchnumber,rt_error, description.of.the.error)), paste(source_folder, "MAASTRO_data_301120_train.dat", sep = "\\"), row.names = FALSE)
write.csv(subset(data_test, select = c(Anatomic_tumor_loc,T_Stage,N_Stage,M_Stage,Treatment_Intent)), paste(source_folder, "MAASTRO_data_301120_test.dat", sep = "\\"), row.names = FALSE)

#save test set in csv
write.table(data_test, paste(source_folder,"approach2_301120_updated_errors_test.csv", sep = "\\"), row.names = FALSE, sep = ';', dec=".")
write.table(data, paste(source_folder,"Hugin-BN-RT-QA.dat", sep = "\\"), row.names = FALSE, sep = ',', dec=".")
