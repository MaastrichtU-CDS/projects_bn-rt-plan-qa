###########################################
# Read Poznan data
###########################################
site <- "Poznan"
data_Poznan_test <- read.csv(paste(source_folder, "", sep = "\\"))

check_binning(binning, data_Poznan_test)

colnames(data_Poznan_test)

data_test_assumed_correct <- data_Poznan_test[c("Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")]
data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
data_test_assumed_correct[data_test_assumed_correct == "\"\""] <- NA
write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, "Hugin-BN-RT-QA-Poznan_test.dat", sep = "\\"), row.names = FALSE)

for(i in 1:length(colnames(data_Poznan_test)))
{
  variable <- colnames(data_Poznan_test)[i]
  print(variable)
  variables_to_exclude <- c(c(1,2,3,5),i)
  if(!(variable %in% c("Researchnumber", "MU", "errors", "errors_description", "Anatomic_Tumor_Location","T_Stage","N_Stage","M_Stage","Treatment_Intent")))
  {
    data_test_assumed_correct <- data_Poznan_test[-variables_to_exclude]   
    data_test_assumed_correct<- replace(data_test_assumed_correct, TRUE, lapply(data_test_assumed_correct, shQuote, type = "cmd"))
    data_test_assumed_correct[data_test_assumed_correct == "\"\""] <- NA
    write.csv(data_test_assumed_correct, na = "", quote = FALSE, paste(source_folder, sub_folder, paste0("Hugin-BN-RT-QA-",site,"_test_",variable,".dat"), sep = "\\"), row.names = FALSE)
  }
}