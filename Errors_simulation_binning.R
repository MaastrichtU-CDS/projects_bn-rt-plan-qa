##############################################
#BN_RTplan data check#########################
#2nd analysis/exclude the values and rounding#
##############################################

library("readxl")

Data <- read_excel("C:\...............\\Data.xlsx")

# Evaluate variables and remove empty columns and create factor variables.
colnames(Data)
str(Data)
Data$Researchnumber <- as.factor(Data$Researchnumber)
Data$PBD <- as.factor(Data$PBD)
Data$AnatomicTumorLoc <- NULL
Data$Diagnose <- as.factor(Data$Diagnose)
Data$cT <- as.factor(Data$cT)
Data$cN <- as.factor(Data$cN)
Data$cM <- as.factor(Data$cM)
Data$TStage <- NULL
Data$NStage <- NULL
Data$MStage <- NULL
Data$AnatomicTumorLoc<-NULL
Data$TreatmentIntent <-as.factor(Data$TreatmentIntent)
Data$Indicatie <- as.factor(Data$Indicatie)
Data$`Zorgplan (Protocol)` <- as.factor(Data$`Zorgplan (Protocol)`)
Data$RxRadiationType <- as.factor(Data$RxRadiationType)
Data$MachineId <- as.factor(Data$MachineId)
Data$PlanTechnique <- as.factor(Data$PlanTechnique)
Data$Bolus <- as.factor(Data$Bolus)
Data$BeamEnergy <- as.factor(Data$BeamEnergy)
Data$Orientation <- as.factor(Data$Orientation)
Data$Tolerance <- as.factor(Data$Tolerance)
Data$NumberOfRxs <-as.factor(Data$NumberOfRxs)
Data$SetupDevice1 <- NULL
Data$SetupDevice2 <- NULL
Data$SetupDevice3 <- NULL
Data$SetupDevice4 <- NULL

# we have 2 variables which indicate treatmentintent, treatmentintent and indication, see how they related
library(ggplot2)
ggplot(Data, aes(x = TreatmentIntent, fill = factor(Indicatie))) +
  geom_bar() +
  xlab("TreatmentIntent") +
  ylab("Total Count") +
  labs(fill = "Indicatie") 
# treatmentintent does not seem correct, almost all patients are recorded as Curative, as they are not. Indication seems more correct (has a mix between palliative and radical(which is same as curative).
# we have another variable to verify, the Zorplan (protocol) variable, palliative procols (often) start with a P- P for palliative, 

# clear TreatmentIntent since it is wrong anyway
Data$TreatmentIntent <-NULL

#check columns again
colnames(Data)

#check Zorplan (protocol)
library(stringr)
Protocol <- Data[which(str_detect(Data$`Zorgplan (Protocol)`, "P-")),]
Protocol[1:5,]

# make a variable presence of "P-"
Data$Palliative <- str_detect(Data$`Zorgplan (Protocol)`, "P-")
Data$Palliative <- as.factor(Data$Palliative)

#check columns again
colnames(Data)

# compare created variable with indicatie variable from HIX
library(ggplot2)
ggplot(Data, aes(x = Palliative, fill = factor(Indicatie))) +
  geom_bar() +
  xlab("Palliative") +
  ylab("Total Count") +
  labs(fill = "Indicatie")




# the protocol based variable palliative is in agreement with indication, however solves a lot of NA in the indication variable.
# The string search makes a mistake for two protocolnames: R-M-LY-S-BP-5x7 AND R-M-LY-S-OP-3x10 which should have been radical. 
# Lets make a new variable based on indicatie and added with Palliative
Data$Treatment_Intent <- ifelse(Data$Indicatie == "Radicaal", "Radicaal", ifelse(Data$Indicatie == "Palliatief", "Palliatief", ifelse(Data$Indicatie == NA, Data$Palliative)))
str(Data)

#Delete indicatie
Data$Indicatie <- NULL
#Delete palliative
Data$Palliative <- NULL
#Check Collimator_Angle
Data$GantryAngle
#check GantryAngle
Data$GantryAngle <- as.double(Data$GantryAngle)
boxplot(Data$GantryAngle)

#check NumberOfRxs wrong all the values are 0
#check Dose per fraction 
Data$DosePerFraction
#check Total fractions
Data$TotalFractions


#check dose per fraction with total fractions
Data$PTVDoseCheck <- Data$DosePerFraction * Data$TotalFractions
Data$PTVDoseCheck

Data$PTVDoseCheck1 <- Data$PTVDoseCheck - Data$PTVDoseRx
Data$PTVDoseCheck1
Data$PTVDoseCheck1 <-NULL
Data$PTVDoseCheck <-NULL


#check RxRadiationType
Data$RxRadiationType
library(ggplot2)
ggplot(Data, aes(x = RxRadiationType)) +
  geom_bar() +
  xlab("RxRadiationType") +
  ylab("Total Count") 

#check PlanTechnique
Data$PlanTechnique
library(ggplot2)
ggplot(Data, aes(x = PlanTechnique)) +
  geom_bar() +
  xlab("PlanTechnique") +
  ylab("Total Count") 

#check TableAngle
Data$TableAngle
boxplot(Data$TableAngle)

#check NumberOfBeams
Data$NumberOfBeams
boxplot(Data$NumberOfBeams)

#check wedge
Data$Wedge
boxplot(Data$Wedge)

#check ControlPoints
Data$ControlPoints
boxplot(Data$ControlPoints)

#check MU_cGy
Data$MU_cGy
boxplot(Data$MU_cGy)

#check GantryAngle
Data$GantryAngle
boxplot(Data$GantryAngle)



#check BeamEnergy
Data$BeamEnergy
Data$BeamEnergy <- as.double(Data$BeamEnergy)
boxplot(Data$BeamEnergy)

#check Orientation
Data$Orientation
Data$Orientation <- as.character(Data$Orientation)
library(ggplot2)
ggplot(Data, aes(x = Orientation)) +
  geom_bar() +
  xlab("Orientation") +
  ylab("Total Count") 

#check CouchLat
Data$CouchLat
boxplot(Data$CouchLat)

#check CouchLong
Data$CouchLong
boxplot(Data$CouchLong)

#check CouchVert
Data$CouchVert
boxplot(Data$CouchVert)

#check Tolerance
Data$Tolerance
library(ggplot2)
ggplot(Data, aes(x = Tolerance)) +
  geom_bar() +
  xlab("Tolerance") +
  ylab("Total Count") 


#Data check
str(Data)


Data$Researchnumber <- as.factor(Data$Researchnumber)
Data$PBD <- as.factor(Data$PBD)
Data$AnatomicTumorLoc <- NULL
Data$Diagnose <- as.factor(Data$Diagnose)
Data$cT <- as.factor(Data$cT)
Data$cN <- as.factor(Data$cN)
Data$cM <- as.factor(Data$cM)
Data$TStage <- NULL
Data$NStage <- NULL
Data$MStage <- NULL
Data$Treatment_Intent <-as.factor(Data$Treatment_Intent)
Data$Diagnose <- as.factor(Data$Diagnose)
Data$`Zorgplan (Protocol)` <- as.factor(Data$`Zorgplan (Protocol)`)
Data$RxRadiationType <- as.factor(Data$RxRadiationType)
Data$MachineId <- as.factor(Data$MachineId)
Data$PlanTechnique <- as.factor(Data$PlanTechnique)
Data$Bolus <- as.factor(Data$Bolus)
Data$BeamEnergy <- as.factor(Data$BeamEnergy)
Data$Orientation <- as.factor(Data$Orientation)
Data$Tolerance <- as.factor(Data$Tolerance)
Data$NumberOfRxs <-as.factor(Data$NumberOfRxs)
Data$SetupDevice1 <- NULL
Data$SetupDevice2 <- NULL
Data$SetupDevice3 <- NULL
Data$SetupDevice4 <- NULL

#check missing values
sum(is.na(Data))

#check how many missing values we have
colSums(is.na(Data))

#create a dataframe with the NAs total missing values of each column
na_count <-sapply(Data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)


#############################################
####   Rx_Radiation_Type ####################
#############################################
#create a new column indicating the radiation type
Data$BeamEnrgyRadiationType = Data$RxRadiationType

#check the new variable
class(Data$BeamEnrgyRadiationType)

#change it to character
Data$BeamEnrgyRadiationType <- as.character(as.factor(Data$BeamEnrgyRadiationType))
require(plyr)
require(dplyr)
revalue(Data$BeamEnrgyRadiationType, c("10X" = "Photons")) -> Data$BeamEnrgyRadiationType
revalue(Data$BeamEnrgyRadiationType, c("12E" = "Electrons")) -> Data$BeamEnrgyRadiationType
revalue(Data$BeamEnrgyRadiationType, c("15E" = "Electrons")) -> Data$BeamEnrgyRadiationType
revalue(Data$BeamEnrgyRadiationType, c("6E" = "Electrons")) -> Data$BeamEnrgyRadiationType
revalue(Data$BeamEnrgyRadiationType, c("6X" = "Photons")) -> Data$BeamEnrgyRadiationType
# revalue(Data$BeamEnrgyRadiationType, c("0P" = "Protons")) -> Data$BeamEnrgyRadiationType


#remove proton therapy patients and electrons 9E
library(dplyr)
Data <- filter(Data,BeamEnrgyRadiationType != "0P")
Data <- filter(Data,BeamEnrgyRadiationType != "9E")

#rename the column according to Luk
names(Data)[names(Data) == "BeamEnrgyRadiationType"] <- "Rx_Radiation_Type"

#delete the column of radiation type we dont need
Data$RxRadiationType <- NULL


table(Data$Rx_Radiation_Type)





#############################################
####  Table Angle     #######################
#############################################

#check the variable
class(Data$TableAngle)
table(Data$TableAngle)
Data$TableAngle<- round(Data$TableAngle)
table(Data$TableAngle)

# 
# #check Luk's variable
# table(LukData$Table_Angle)
# summary(LukData$Table_Angle)

library(tidyverse)



#############################################
#### Control points##########################
#############################################

#check the variable
class(Data$ControlPoints)
table(Data$ControlPoints)




#############################################
#### Collimator angle #######################
#############################################

#check the collimator angle
table(Data$CollimatorAngle)
#round the values
Data$CollimatorAngle <- round(Data$CollimatorAngle) # Round off the column t

#############################################
####   Gantry Angle #########################
#############################################
table(Data$GantryAngle)
Data$GantryAngle<- round(Data$GantryAngle)

#############################################
#### clinical T    ##########################
#############################################

#check variable
table(Data$cT)

#change it to character
Data$cT <- as.character(as.factor(Data$cT))

revalue(Data$cT, c("0" = "T0")) -> Data$cT
revalue(Data$cT, c("1" = "T1")) -> Data$cT
revalue(Data$cT, c("1a" = "T1")) -> Data$cT
revalue(Data$cT, c("1a2" = "T1")) -> Data$cT
revalue(Data$cT, c("1b" = "T1")) -> Data$cT
revalue(Data$cT, c("1b1" = "T1")) -> Data$cT
revalue(Data$cT, c("1b2" = "T1")) -> Data$cT
revalue(Data$cT, c("1c" = "T1")) -> Data$cT
revalue(Data$cT, c("1mi" = "T1")) -> Data$cT
revalue(Data$cT, c("2" = "T2")) -> Data$cT
revalue(Data$cT, c("2a" = "T2")) -> Data$cT
revalue(Data$cT, c("2b" = "T2")) -> Data$cT
revalue(Data$cT, c("2c" = "T2")) -> Data$cT
revalue(Data$cT, c("3" = "T3")) -> Data$cT
revalue(Data$cT, c("3a" = "T3")) -> Data$cT
revalue(Data$cT, c("3b" = "T3")) -> Data$cT
revalue(Data$cT, c("3c" = "T3")) -> Data$cT
revalue(Data$cT, c("4" = "T4")) -> Data$cT
revalue(Data$cT, c("4a" = "T4")) -> Data$cT
revalue(Data$cT, c("4b" = "T4")) -> Data$cT
revalue(Data$cT, c("4d" = "T4")) -> Data$cT
revalue(Data$cT, c("is" = "Tis(")) -> Data$cT
revalue(Data$cT, c("IS" = "Tis(")) -> Data$cT
revalue(Data$cT, c("is (dcis)" = "Tis(")) -> Data$cT
revalue(Data$cT, c("is (lcis)" = "Tis(")) -> Data$cT
revalue(Data$cT, c("is (Paget)" = "Tis(")) -> Data$cT
revalue(Data$cT, c("n.i." = "NULL")) -> Data$cT
revalue(Data$cT, c("X" = "TX")) -> Data$cT
revalue(Data$cT, c("Tis(" = "Tis")) -> Data$cT

#rename the column
names(Data)[names(Data) == "cT"] <- "T_stage"
table(Data$T_stage)
#############################################
#### clinical n     ##############
#############################################

#check variable
table(Data$cN)
class(Data$cN)
#check Luk's variable
# table(LukData$N_Stage)

#change it to character
Data$cN <- as.character(as.factor(Data$cN))

revalue(Data$cN, c("0" = "N0")) -> Data$cN
revalue(Data$cN, c("1" = "N1")) -> Data$cN
revalue(Data$cN, c("1a" = "N1")) -> Data$cN
revalue(Data$cN, c("1b" = "N1")) -> Data$cN
revalue(Data$cN, c("1mi" = "N1")) -> Data$cN
revalue(Data$cN, c("2" = "N2")) -> Data$cN
revalue(Data$cN, c("2a" = "N2")) -> Data$cN
revalue(Data$cN, c("2b" = "N2")) -> Data$cN
revalue(Data$cN, c("2c" = "N2")) -> Data$cN
revalue(Data$cN, c("3" = "N3")) -> Data$cN
revalue(Data$cN, c("3a" = "N3")) -> Data$cN
revalue(Data$cN, c("3b" = "N3")) -> Data$cN
revalue(Data$cN, c("3c" = "N3")) -> Data$cN
revalue(Data$cN, c("n.i." = "NULL")) -> Data$cN
revalue(Data$cN, c("X" = "NX")) -> Data$cN
revalue(Data$cN, c("1c" = "N1")) -> Data$cN

#rename the column
names(Data)[names(Data) == "cN"] <- "N_stage"
table(Data$N_stage)

#############################################
#### clinical m     #########################
#############################################

#check variable
table(Data$cM)
class(Data$cM)

#check Luk's variable
# table(LukData$M_Stage)

#change it to character
Data$cM <- as.character(as.factor(Data$cM))

#change the values
revalue(Data$cM, c("0" = "M0")) -> Data$cM
revalue(Data$cM, c("1" = "M1")) -> Data$cM
revalue(Data$cM, c("1a" = "M1")) -> Data$cM
revalue(Data$cM, c("1b" = "M1")) -> Data$cM
revalue(Data$cM, c("1c" = "M1")) -> Data$cM
revalue(Data$cM, c("n.i." = "NULL")) -> Data$cM
revalue(Data$cM, c("X" = "MX")) -> Data$cM

names(Data)[names(Data) == "cM"] <- "M_stage"

table(Data$M_stage)
# #############################################
# #### NumberOfRxs    #########################
# #############################################
# 
table(Data$PBD)
str(Data$PBD)
Data$PBD<-as.character(Data$PBD)
Data$NumberOfRxs = str_sub(Data$PBD,-3)
names(Data)[names(Data) == "PBD"] <- "NumberOfRxs"

table(Data$NumberOfRxs)
Data$NumberOfRxs <- as.character(as.factor(Data$NumberOfRxs))
str(Data$NumberOfRxs)
Data <- subset(Data, !(NumberOfRxs %in% c("33 X 2 GY",  "TEST" )))
Data <- subset(Data, !(NumberOfRxs %in% c("MATCH 27/11", 
                                          "MATCHPLAN",
                                          "MATCHPLAN B",
                                          "MATCHPLAN2",
                                          "MATCHPLANB",
                                          "NIEUW MATCHPL",
                                          "PLAN1",
                                          "PLAN3",
                                          "QA_4CMSHIFT",
                                          "TEST2")))

revalue(Data$NumberOfRxs, c("P1B1D1A_ALD" = "P1B1D1A")) -> Data$NumberOfRxs
revalue(Data$NumberOfRxs, c("P1B1D1AOUD_2" = "P1B1D1A")) -> Data$NumberOfRxs
revalue(Data$NumberOfRxs, c("P1B2D1A_ALD" = "P1B2D1A")) -> Data$NumberOfRxs
revalue(Data$NumberOfRxs, c("P1B2D1A_QA" = "P1B2D1A")) -> Data$NumberOfRxs
revalue(Data$NumberOfRxs, c("P1B3D1B_ALD" = "P1B3D1B")) -> Data$NumberOfRxs
revalue(Data$NumberOfRxs, c("P1B4D1A_ALD" = "P1B4D1A")) -> Data$NumberOfRxs

library(stringr)


#keep the last two characters 
Data$NumberOfRxs <- substr(Data$NumberOfRxs, nchar(Data$NumberOfRxs)-1, nchar(Data$NumberOfRxs))

#keep the first character
Data$NumberOfRxs = substr(Data$NumberOfRxs,1,1)

table(Data$NumberOfRxs)

####  Dose per fraction    ##################
#############################################
table(Data$DosePerFraction)
Data$DosePerFraction <- Data$DosePerFraction *100 #transform it to cGy
Data$DosePerFraction<- round(Data$DosePerFraction)
names(Data)[names(Data) == "DosePerFraction"] <- "Dose_Per_Fraction"

table(Data$Dose_Per_Fraction)

#############################################
####  PTV DOSE RX           #################
#############################################
table(Data$PTVDoseRx)
Data$PTVDoseRx <-Data$PTVDoseRx *100 #transform it to cGy
# library(openxlsx)
# write.xlsx(Data, "C:/Users/pkale/Desktop/checkptv.xlsx")
# Data$PTVDoseRx<- round(Data$PTVDoseRx)
# write.xlsx(Data, "C:/Users/pkale/Desktop/DataNumericalValues.xlsx")

Data$PTVDoseRx <- as.character(as.numeric(Data$PTVDoseRx))
Data$PTVDoseRx <- as.numeric(as.character(Data$PTVDoseRx))
names(Data)[names(Data) == "PTVDoseRx"] <- "PTV_Dose_Rx"
table(Data$PTV_Dose_Rx)
#############################################
####  Total Fractions    ####################
#############################################
table(Data$TotalFractions)

#############################################
#### plan technique     ##############
#############################################
table(Data$PlanTechnique)
Data$PlanTechnique <- as.character(as.factor(Data$PlanTechnique))

Data <- filter(Data,PlanTechnique != "MODULAT_SCANNING")

revalue(Data$PlanTechnique, c("ARC" = "VMAT")) -> Data$PlanTechnique
revalue(Data$PlanTechnique, c("STATIC" = "IMRT")) -> Data$PlanTechnique
revalue(Data$PlanTechnique, c("SRS STATIC" = "IMRT")) -> Data$PlanTechnique
revalue(Data$PlanTechnique, c("SRS ARC" = "VMAT")) -> Data$PlanTechnique
# revalue(Data$PlanTechnique, c("MODULAT_SCANNING" = "NULL")) -> Data$PlanTechnique
names(Data)[names(Data) == "PlanTechnique"] <- "Plan_Technique"

table(Data$Plan_Technique)
#############################################
####  Number of beams       #################
#############################################
table(Data$NumberOfBeams)
# table(LukData$Number_of_beams)
#Data$Number_of_beams <- as.numeric(as.factor(Data$Number_of_beams))

names(Data)[names(Data) == "NumberOfBeams"] <- "Number_of_beams"

table(Data$Number_of_beams)



#############################################
#### wedges        ##############
#############################################
table(Data$Wedge)


#############################################
####   Bolus ################################
#############################################

#check the bolus column now
table(Data$Bolus)
class(Data$Bolus)

#change the column from factor to charater
Data$Bolus <- as.character(as.factor(Data$Bolus))
Data$Bolus <- as.factor(as.character(Data$Bolus))

revalue(Data$Bolus, c("#MEERWAARDEN" = "Y")) -> Data$Bolus


revalue(Data$Bolus, c("Y" = "Yes")) -> Data$Bolus
revalue(Data$Bolus, c("N" = "No")) -> Data$Bolus


#############################################
####   Beam energy       ####################
#############################################

#let's check the variable Beam Energy
table(Data$BeamEnergy)
class(Data$BeamEnergy)
Data$BeamEnergy <- as.character(as.numeric(Data$BeamEnergy))


#change the column from character to numeric
Data$BeamEnergy <- as.numeric(as.character(Data$BeamEnergy))

#rename the column according to Luk
# names(Data)[names(Data) == "BeamEnergy"] <- "Beam_Energy"
# table(Data$Beam_Energy)

Data$BeamEnergy <- revalue(as.character(Data$BeamEnergy), c("6" = "15") )
Data$BeamEnergy <- revalue(as.character(Data$BeamEnergy), c("2" = "6"))                                                 
Data$BeamEnergy <- revalue(as.character(Data$BeamEnergy), c("4" = "10"))  
Data$BeamEnergy <- revalue(as.character(Data$BeamEnergy), c("5" = "12"))  

#change the column from character to numeric
Data$BeamEnergy <- as.numeric(as.character(Data$BeamEnergy))

#rename the column according to Luk
names(Data)[names(Data) == "BeamEnergy"] <- "Beam_Energy"
table(Data$Beam_Energy)

#############################################
#### orientation     ##############
#############################################
table(Data$Orientation)
Data$Orientation <- as.character(as.factor(Data$Orientation))


class(Data$Orientation)
# Data <- filter(Data,Orientation != 'Head First-Decubitus Right')
# Data$Orientation <- revalue(as.character(Data$Orientation), c("Head First-Supine" = "HeadIn-Supine") )
# Data$Orientation <- revalue(as.character(Data$Orientation), c("Head First-Decubitus Right'" = "HeadIn-Supine") )

Data <- subset(Data, !(Orientation %in% c("Head First-Decubitus Right" )))


#############################################
####  tolerance          #################
#############################################
table(Data$Tolerance)
Data$Tolerance <- as.character(as.factor(Data$Tolerance))

Data$Tolerance <- revalue(as.character(Data$Tolerance), c("Console RUIM" = "Others"))



#############################################
####  treatment intent         ##############
#############################################
class(Data$Treatment_Intent)
table(Data$Treatment_Intent)

Data$Treatment_Intent <- revalue(as.character(Data$Treatment_Intent), c("Palliatief" = "Palliative"))  

Data$Treatment_Intent <- revalue(as.character(Data$Treatment_Intent), c("Radicaal" = "Curative"))  




#############################################
#### anatomic tumour location   ##############
#############################################
Data$Diagnose <- NULL
names(Data)[names(Data) == "Diagnose_eng"] <- "Diagnose"

table(Data$Diagnose)
Data$Diagnose <- as.character(as.factor(Data$Diagnose))
class(Data$Diagnose)
library(plyr)

revalue(Data$Diagnose, c("malignant neoplasm of lung" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant melanoma of ear" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of esophagus" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of Breast" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of prostate" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of tongue" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of skin of face" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of parotid gland" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of mouth" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of skin" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of larynx" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of oropharynx" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of nasal cavity" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of bladder" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of brains" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of thyroid" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("metastasis" = "METASTASIS")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of hypopharynx" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of floor of mouth" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of supraglottis" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of base of tongue" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of nasopharynx" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of tonsil" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of kidney" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse large cell non-Hodgkin's Lymphoma" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of posterior wall of the oropharynx" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of skin of upper limb" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("follicular non-Hodgkin's Lymphoma" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasms of submandibular" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("extranodal marginal zone B-cell Lymphoma of mucosa-associated lymphoid tissue" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of vallecula epiglottica" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant melanoma of the face" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of skin of neck" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of maxillary sinus" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Lymphomatoid palpulose" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of larynx" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of upper gum" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of the head and neck" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("plasmocytoom solitary bone" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of major salivary glands" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("prostate" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("tongue base" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("kidney" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("rectum" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("thymus, anterior mediastinum" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("lung, bronchus" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of rectum" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("lung, upper lobe" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissue" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of sigmoid colon" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ascending colon" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of pancreas" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("meningioma" = "Head and Neck (mainly a brain tumour but can be categorised in the Head and Neck group)")) -> Data$Diagnose
revalue(Data$Diagnose, c("myelosarcoom" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("hepatocellular carcinoma" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("non-Hodgkin's Lymphoma " = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of brain" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of appendix" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant melanoma of upper limb" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("neurinoma" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of rectum in transition from sigmoid" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of stomach" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of Breast" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant melanoma of neck" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("glioblastoma" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of olfactory nerve" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of thymus" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of vulva" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of opinions" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("intraductal carcinoma in situ of Breast" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of uterus" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant melanoma of lower limb" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant melanoma of skin" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of esophagus" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of pelvic" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ovary" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of endocervix" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of vagina" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of fallopian tube" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of colon" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of skin of lower limb" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("T-cell Lymphoma" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("non-small cell lung cancer" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("primary cutaneous anaplastic large T-cell Lymphoma, CD30 positive" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of renal pelvis\" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of anus" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of sinus ethmoidalis" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("primarily diffuse large B-cell Lymphoma by central nervous system" = "Central nervous system (CNS)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of testis" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("lobular carcinoma in situ of Breast" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of duodenum" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("arthrosis" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of descending colon" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of lower limb" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of pituitary" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of gastric fundus" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("diagnostics still underway @DBC" = "UNKNOWNPRIMARY")) -> Data$Diagnose
revalue(Data$Diagnose, c("Pleural Mesothelioma" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of gallbladder" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of sinus" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of cranial nerve" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of small intestine" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of transverse colon" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of eye" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of biliary" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("nodular lymphocyte-rich Hodgkin's Lymphoma" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of exocervix" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("low-grade glioma of brains" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("intrahepatic bile duct carcinoma" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of cerebral opinions" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of cecum" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of eyelid" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of liver" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of body of pancreas" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissue of upper limb" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse non-Hodgkin's Lymphoma" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse small cell non-Hodgkin's Lymphoma" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Primary malignant neoplasm of cerebrum" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of bone" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Acute myeloid leukemia FLT3 mutation" = "LEUKEMIA")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of glans penis" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissue of the abdomen" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("heterotopic ossification" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of the bladder" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of brains, supratentorial" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("oligodendroglioma" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("keloid scar" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Ewing's sarcoma of bone" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of skin of trunk" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of the back" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of endocervix" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("impairment of saliva" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of middle ear" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("kaposi's sarcoma of soft tissue" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Other and unspecified gastroenteritis of infectious origin" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ear" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("urothelial cell carcinoma of kidney" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Carcinoma with osteoclast-like giant cells" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("chronic myelomonocytic leukemia" = "LEUKEMIA")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of cauda of pancreas" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Kaposi's sarcoma" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of major duodenal papilla" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("plasma cell myeloma " = "HEMATOPOIETIC /cancer of plasma cells")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ductus craniopharyngeus" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("invasive malignant neoplasm of bladder" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom NNE" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("corpus uteri" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("brains, temporal lobe" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom, medial upper quadrant" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("bronchus / lung NNE" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("esophagus, thoracic" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("lung, lower lobe" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("Other var. (Eg.prim.cutaan CD30 grootcell.Tcel.lymf)" = "UNKNOWNPRIMARY")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom, central" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("thymus" = "Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("uterus NNE" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant bone marrow tumor NNE\" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("metastases with unknown primary tumor\" = "METASTASIS")) -> Data$Diagnose
revalue(Data$Diagnose, c("plasmocytoom NNE" = "HEMATOPOIETIC")) -> Data$Diagnose
revalue(Data$Diagnose, c("skin, lower extremities" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("esophagus, abdominal part\" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("cervix uteri NNE" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom, medial quadrant" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("Hodgkin Lymphoma (lymphocyte rich form)" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("ovary" = "Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("soft tissue, lower limb / Hip" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Other localizations brains" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("brains, parietal lobe" = "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("colon, sigmoid" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("mom, axillary spur" = "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("primary cutaneous diffuse large B-cell Lymphoma of bone" = "Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("transition rectum / sigmoid" = "Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("melanoma of the skin, skull / neck / nape" = "Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("reticulosarcoma" = "Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("paranasal sinuses, frontal sinus" = "Head and Neck")) -> Data$Diagnose



revalue(Data$Diagnose, c("acute lymphoblastic leukemia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("acute myeloid leukemia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("acute myeloid leukemia with inv (3) (q21q26.2) or t (3; 3) (q21q26.2) [RPN1-EVI1]"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("acute myeloid leukemia with t (8; 21) (q22; q22) [RUNX-RUNX1T1]"Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("anaplastic large cell lymphoma, ALK"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("aplastic anemia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("B-cell lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Bacterial, viral and other infectious agents"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign neoplasm of brains"= "Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of breast= "Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("chronic lymphocytic leukemia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("chronic lymphocytic leukemia / small lymphocytic lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("chronic lymphoid leukemia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("coxarthrosis"="Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Diagnosis n.n.o. @DBC"="Unknown primary")) -> Data$Diagnose
revalue(Data$Diagnose, c("diagnosis not thesaurus, PLEASE fill"="Unknown primary")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse large cell non-Hodgkin's lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse non-Hodgkin's lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("diffuse small cell non-Hodgkin's lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("endometrial carcinoma in situ"="Gynecologic (GYN)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Epilepsy: Epilepsy surgery and cortical motor stimulation @DBC"="Central nervous system (CNS)")) -> Data$Diagnose
revalue(Data$Diagnose, c("esophagus, abdominal part"="Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("extranodal marginal zone B-cell lymphoma of mucosa-associated lymphoid tissue"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Follicular - Nodular lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("follicular non-Hodgkin's lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("emengdcellige Hodgkin's disease"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Head and Neck (mainly a brain tumour but can be categorised in the Head and Neck group)"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("HEMATOPOIETIC"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("Hodgkin's lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Hodgkin lymphoma (lymphocyte rich form)"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("intraductal carcinoma in situ of breast"="Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("Kahler's disease"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("LEUKEMIA"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("lobular carcinoma in situ of breast"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("lymphomatoid palpulose"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("lymphosarcoma"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant bone marrow tumor NNE"="Central nervous system (CNS)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant melanoma of the trunk"="Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of buccal"="Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of cardia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of cerebral opinions"="Central nervous system (CNS)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of cervix uteri"="Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of endometrium"="Gynecologic (GYN)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of glottis"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of gums of mandible"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of lacrimal duct"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of mandible"="Head and Neck" )) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of maxilla"="Head and Neck" )) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of opinions"="Head and Neck" )) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of renal pelvis"="Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of sinus pyriformis"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of ureter"="Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("mantle"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("metastases with unknown primary tumor"="METASTASIS")) -> Data$Diagnose
revalue(Data$Diagnose, c("mum, lateral lower quadrant"="Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("mum, lateral upper quadrant"="Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("mycosis fungoides"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("myelodysplastic syndrome"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("nodal marginal zone B-cell lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("nodular lymphocyte-rich Hodgkin's lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("nodular sclerosing Hodgkin's disease"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("non-Hodgkin's lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("plantar fasciitis"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("plasmacelmyeloom"="Hematopoietic" )) -> Data$Diagnose
revalue(Data$Diagnose, c("primarily diffuse large B-cell lymphoma by central nervous system"="Lymphoma" )) -> Data$Diagnose
revalue(Data$Diagnose, c("primary cutaneous anaplastic large T-cell lymphoma, CD30 positive"="Lymphoma" )) -> Data$Diagnose
revalue(Data$Diagnose, c("primary cutaneous diffuse large B-cell lymphoma of bone"="Lymphoma" )) -> Data$Diagnose
revalue(Data$Diagnose, c("primary cutaneous follicular centrumcellymfoom"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Primary malignant neoplasm of alveolar mucosa"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("primary myelofibrosis"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("refractory cytopenia with multi-lineage dysplasia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("T-cell lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("UNKNOWNPRIMARY"="Unknown primary")) -> Data$Diagnose
revalue(Data$Diagnose, c("vocal cords"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Waldenstrom's macroglobulinemia"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("acute myeloid leukemia with t (8; 21) (q22; q22) [RUNX-RUNX1T1]"="Hematopoietic ")) -> Data$Diagnose

revalue(Data$Diagnose, c("anaplastic astrocytoma brains"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("B-cell prolymfocytenleukemie"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("B lymphoblastic leukemia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("benign lesion meninges"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("brains NNE"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of breast"="Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("carcinoma in situ of prostate"="Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("esophagus, lower 1/3"="Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("extramedullary plasmacytoma"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("gemengdcellige Hodgkin's disease"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("hepatosplenaal T-cell lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("infratentorial benign neoplasm of brains"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Kaposi's sarcoma of skin"="Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Kaposi's sarcoma of skin"="Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of adrenal gland"="Central nervous system (CNS)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of adrenal gland"="Central nervous system (CNS)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of breast"="Breast")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of bronchus"="Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of carotid body"="Musculoskeletal (MSK)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of corpus uteri"="Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of endocrine pancreas"="Central nervous system (CNS)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of extrahepatic bile ducts"="Gastrointestinal (GI)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of lateral wall of oropharynx"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of lip"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of minor salivary gland"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of penis"="Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("malignant neoplasm of soft tissues of the thorax"="Lung")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of urachus"="Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("Malignant neoplasm of urethra"="Genitourinary (GU)")) -> Data$Diagnose
revalue(Data$Diagnose, c("melanoma skin NNE"="Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("METASTASIS"="Metastasis")) -> Data$Diagnose
revalue(Data$Diagnose, c("nasal cavities"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("nasal extranodal NK / T-cell lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("Other localizations mum"="Unknown primary")) -> Data$Diagnose
revalue(Data$Diagnose, c("pituitary adenoma"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("plasma blas dramatically lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("primary mediastinal large B-cell lymphoma"="Lymphoma")) -> Data$Diagnose
revalue(Data$Diagnose, c("screening heart transplant recipient"="Unknown primary")) -> Data$Diagnose
revalue(Data$Diagnose, c("skin, fuselage"="Skin")) -> Data$Diagnose
revalue(Data$Diagnose, c("submandibular"="Head and Neck")) -> Data$Diagnose
revalue(Data$Diagnose, c("T lymphoblastic leukemia"="Hematopoietic")) -> Data$Diagnose
revalue(Data$Diagnose, c("urothelial cell carcinoma of the bladder"="Genitourinary (GU)")) -> Data$Diagnose

revalue(Data$Diagnose, c("#VALUE!"="Unknown primary")) -> Data$Diagnose
                         revalue(Data$Diagnose, c("Hematopoietic "="Hematopoietic")) -> Data$Diagnose
 
                         names(Data)[names(Data) == "Diagnose"] <- "Anatomic_tumour_location"
                         table(Data$Anatomic_tumour_location)
                         
                         
                         ##########################################
                         ######ID_PBD##############################
                         ##########################################
                         # Let's create a seperate IDPBD column to seperate the different plans for each plan
                         
                         Data$IDPBD <- paste(Data$Researchnumber,Data$PBD)
                         
                         #now let's create a separete df from the computation
                         
                         DattaMU <- Data[, c("Researchnumber", "IDPBD", "Mu","PTV_Dose_Rx","GantryAngle","GantryDirection","GantryStopAngle")]
                         
                         DattaMU$Mu<- round(DattaMU$Mu)
                         DataSumMu<-aggregate(DattaMU$Mu, by=list(IDPBD=DattaMU$IDPBD), FUN=sum)
                         DataSumMuFINAL <- merge(DattaMU,DataSumMu, by  = "IDPBD") 
                         
                         DataSumMuFINAL$MU_cGy <-DataSumMuFINAL$x / DataSumMuFINAL$PTV_Dose_Rx
                         
                         
                         DataSumMuFINAL$difference <- DataSumMuFINAL$GantryStopAngle - DataSumMuFINAL$GantryAngle
                         
                         DataSumMuFINAL$difference <- abs(DataSumMuFINAL$difference)
                         DataSumMuFINAL$MU_deg <-DataSumMuFINAL$difference / DataSumMuFINAL$Mu
                         
                         
                       
                         
                         
                         
                         
                         ################################
                         ###### add the columns we created to the initial dataframe
                         ################################
                         
                         #delete the columns I do not need
                         DataSumMuFINAL$Mu <- NULL
                         DataSumMuFINAL$PTV_Dose_Rx <- NULL
                         DataSumMuFINAL$GantryAngle <- NULL
                         DataSumMuFINAL$GantryDirection <- NULL
                         DataSumMuFINAL$GantryStopAngle <- NULL
                         DataSumMuFINAL$x <- NULL
                         DataSumMuFINAL$difference <- NULL
                         
                         
                         
                         updated_data<-as.data.frame(cbind.data.frame(DataSumMuFINAL, Data))
                         updated_data$IDPBD<-NULL
                         updated_data$Researchnumber<-NULL
                         
                         # updated_data <- cbind(DataSumMuFINAL,Data, by = 'Researchnumber')
                         
                         
                         
                         
                         # #############################################
                         # ####  MU_deg    ###############################
                         # #############################################
                         table(updated_data$MU_deg)
                         updated_data$MU_deg<- round(updated_data$MU_deg, digits = 2)
                         
                             
                         
                         # #############################################
                         # ####  MU_cGy     ###############################
                         # #############################################
                         #
                         #check variable
                         table(updated_data$MU_cGy)
                         class(updated_data$MU_cGy)
                         updated_data$MU_cGy<- round(updated_data$MU_cGy, digits = 2)
                         #
                         updated_data$MU_cGy <- as.numeric(as.character(updated_data$MU_cGy ))
                         
                                
                         
                         ###############
                         ###ssd THE CORRECT ONE FOR THE SSD
                         ########
                         table(updated_data$SSD)
                         updated_data$SSD<- round(updated_data$SSD)
                         
                         
                         
                         updated_data$SSD <- as.numeric(as.character(updated_data$SSD ))
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         ##########################################################################
                         ######################## Final cleaning###################################
                         ##########################################################################
                         colnames(updated_data)
                         table(updated_data$T_stage)
                         table(updated_data$N_stage)
                         table(updated_data$M_stage)
                         table(updated_data$NumberOfRxs)
                         names(updated_data)[names(updated_data) == "NumberOfRxs"] <- "Number_of_Rxs"
                         table(updated_data$Number_of_Rxs)
                         table(updated_data$Dose_Per_Fraction)
                         table(updated_data$PTV_Dose_Rx)
                         table(updated_data$TotalFractions)
                         names(updated_data)[names(updated_data) == "TotalFractions"] <- "Total_Fractions"
                         table(updated_data$Total_Fractions)
                         updated_data$MachineId <- NULL
                         table(updated_data$Plan_Technique)
                         table(updated_data$TableAngle) #THE CORRECT ONE FOR Table_Angle
                         names(updated_data)[names(updated_data) == "TableAngle"] <- "Table|_Angle"
                         table(updated_data$Number_of_beams) 
                         table(updated_data$Wedge) 
                         table(updated_data$ControlPoints) #THE CORRECT ONE FOR Control_Points
                         table(updated_data$SSD)#THE CORRECT ONE FOR SSD
                         table(updated_data$Bolus)
                         # revalue(updated_data$Bolus, c("#MEERWAARDEN" = "Yes")) -> updated_data$Bolus
                         table(updated_data$Bolus)
                         table(updated_data$Bolus)
                         table(updated_data$GantryAngle) #THE CORRECT ONE FOR Gantry_Angle
                         table(updated_data$CollimatorAngle)
                         table(updated_data$Beam_Energy)
                         table(updated_data$Orientation)
                         updated_data$CouchLat <-NULL
                         updated_data$CouchLong <-NULL
                         updated_data$CouchVert<-NULL
                         table(updated_data$Tolerance)
                         table(updated_data$Treatment_Intent)
                         table(updated_data$Rx_Radiation_Type)
                         table(updated_data$MU_cGy) #THE CORRECT ONE FOR mu/cGy
                         table(updated_data$MU_deg) #THE CORRECT ONE FOR mu/deg
                         names(updated_data)[names(updated_data) == "Anatomic_tumour_location"] <- "Anatomic_tumour_loc"
                         updated_data$`Zorgplan (Protocol)`<-NULL
                         updated_data$NumberOfRxs.1<-NULL
                         updated_data$IDPBD<-NULL
                         
                         
                         
                         
                         
                            
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                                
                         # library("readxl")
                         updated_data<- read_excel('C:\.......................\updated_network_data_130622b.xlsx')
                         table(updated_data$Number_of_Rxs...4)
                         table(updated_data$Number_of_Rxs...9)
                         
                         
                         #remove unnecessary columns
                         updated_data$Number_of_Rxs...9<-NULL
                         names(updated_data)[names(updated_data) == "Number_of_Rxs...4"] <- "Number_of_Rxs"
                         
                         
                         updated_data$GantryDirection<-NULL
                         updated_data$GantryStopAngle<-NULL
                         
                         colnames(updated_data)
                         
                           ###############################################################
                         ###############################################################
                         ########################ERRORS SIMULATION######################
                         ###############################################################
                         ###############################################################
                         
                         ##############Bolus errors#####################################
                         updated_data$Bolus_errors<- (updated_data$Bolus)
                         # updated_data$Bolus_errors <- gsub("\\'", "", updated_data$Bolus_errors)
                         # as.data.frame(sapply(df, function(updated_data) gsub("\"", "", x)))
                         library(plyr)
                         table(updated_data$Bolus_errors)
                         updated_data$Bolus_errors[sample(which(updated_data$Bolus_errors == "Yes"), 30)] <-"Bolus Should have been No"
                         updated_data$Bolus_errors[sample(which(updated_data$Bolus_errors == "No"), 30)] <-"Bolus Should have been Yes"
                         updated_data$Bolus_errors_categorical<-ifelse(updated_data$Bolus==updated_data$Bolus_errors,"No","Error")
                         table(updated_data$Bolus_errors_categorical)
                         
                         ##############Beam energy errors#####################################
                         table(updated_data$Beam_Energy)
                         updated_data$Beam_Energy_errors<- (updated_data$Beam_Energy)
                         library(plyr)
                         table(updated_data$Beam_Energy_errors)
                         updated_data$Beam_Energy_errors[sample(which(updated_data$Beam_Energy_errors ==6), 30)] <-"Beam energy Should have been 10"
                         updated_data$Beam_Energy_errors[sample(which(updated_data$Beam_Energy_errors ==10), 30)] <-"Beam energy Should have been 6"
                         table(updated_data$Beam_Energy_errors)
                         updated_data$Beam_Energy_errors_categorical<-ifelse(updated_data$Beam_Energy==updated_data$Beam_Energy_errors,"No","Error")
                         table(updated_data$Beam_Energy_errors_categorical)
                         
                         
                         ##############Radiation type errors#####################################
                         table(updated_data$Rx_Radiation_Type)
                         updated_data$Rx_Radiation_Type_errors<- (updated_data$Rx_Radiation_Type)
                         library(plyr)
                         table(updated_data$Rx_Radiation_Type_errors)
                         updated_data$Rx_Radiation_Type_errors[sample(which(updated_data$Rx_Radiation_Type_errors =="Electrons"), 30)] <-"Rx_Radiation_Type Should have been Photons"
                         updated_data$Rx_Radiation_Type_errors[sample(which(updated_data$Rx_Radiation_Type_errors =="Photons"), 30)] <-"Rx_Radiation_Type Should have been Electrons"
                         table(updated_data$Rx_Radiation_Type_errors)
                         updated_data$Rx_Radiation_Type_errors_categorical<-ifelse(updated_data$Rx_Radiation_Type==updated_data$Rx_Radiation_Type_errors,"No","Error")
                         table(updated_data$Rx_Radiation_Type_errors_categorical)
                         
                         
                         ##############Number of beams error#####################################
                         table(updated_data$Number_of_beams)
                         updated_data$Number_of_beams_errors<- (updated_data$Number_of_beams)
                         library(plyr)
                         table(updated_data$Number_of_beams_errors)
                         updated_data$Number_of_beams_errors[sample(which(updated_data$Number_of_beams_errors ==4), 50)] <-"Number_of_beams Should have been 6"
                         updated_data$Number_of_beams_errors[sample(which(updated_data$Number_of_beams_errors ==6), 60)] <-"Number_of_beams Should have been 8"
                         table(updated_data$Number_of_beams_errors)
                         updated_data$Number_of_beams_errors_categorical<-ifelse(updated_data$Number_of_beams==updated_data$Number_of_beams_errors,"No","Error")
                         table(updated_data$Number_of_beams_errors_categorical)
                         
                         
                         
                         ##############ssd error#####################################
                         table(updated_data$SSD)
                         updated_data$SSD_errors<- (updated_data$SSD)
                         library(plyr)
                         table(updated_data$SSD_errors)
                         updated_data$SSD_errors[sample(which(updated_data$SSD_errors ==80), 20)] <-"SSD Should have been 90"
                         updated_data$SSD_errors[sample(which(updated_data$SSD_errors ==120), 30)] <-"SSD Should have been 100"
                         table(updated_data$SSD_errors)
                         updated_data$SSD_errors_categorical<-ifelse(updated_data$SSD==updated_data$SSD_errors,"No","Error")
                         table(updated_data$SSD_errors_categorical)
                         
                         
                         ##############collimator angle error#####################################
                         table(updated_data$CollimatorAngle)
                         updated_data$CollimatorAngle_errors<- (updated_data$CollimatorAngle)
                         library(plyr)
                         table(updated_data$CollimatorAngle_errors)
                         updated_data$CollimatorAngle_errors[sample(which(updated_data$CollimatorAngle_errors ==80), 20)] <-"Collimator Angle Should have been 110"
                         updated_data$CollimatorAngle_errors[sample(which(updated_data$CollimatorAngle_errors ==330), 40)] <-"Collimator Angle Should have been 355"
                         table(updated_data$CollimatorAngle_errors)
                         updated_data$CollimatorAngle_errors_categorical<-ifelse(updated_data$CollimatorAngle==updated_data$CollimatorAngle_errors,"No","Error")
                         table(updated_data$CollimatorAngle_errors_categorical)
                         
                         
                         ##############Gantry angle error#####################################
                         table(updated_data$GantryAngle)
                         updated_data$GantryAngle_errors<- (updated_data$GantryAngle)
                         library(plyr)
                         table(updated_data$GantryAngle_errors)
                         updated_data$GantryAngle_errors[sample(which(updated_data$GantryAngle_errors ==30), 20)] <-"Gantry Angle Should have been 0"
                         updated_data$GantryAngle_errors[sample(which(updated_data$GantryAngle_errors ==0), 40)] <-"Gantry Angle Should have been 20"
                         table(updated_data$GantryAngle_errors)
                         updated_data$GantryAngle_errors_categorical<-ifelse(updated_data$GantryAngle==updated_data$GantryAngle_errors,"No","Error")
                         table(updated_data$GantryAngle_errors_categorical)
                         
                         
                         ##############table angle error#####################################
                         table(updated_data$`Table|_Angle`)
                         updated_data$Table_Angle_errors<- (updated_data$`Table|_Angle`)
                         library(plyr)
                         table(updated_data$Table_Angle_errors)
                         updated_data$Table_Angle_errors[sample(which(updated_data$Table_Angle_errors ==350), 20)] <-"Table Angle Shpuld have been 0"
                         updated_data$Table_Angle_errors[sample(which(updated_data$Table_Angle_errors ==0), 25)] <-"Table Angle Should have been 10"
                         # updated_data$Table_Angle_errors[sample(which(updated_data$Table_Angle_errors =='"[1-11]"'), 35)] <-'"0"'
                         
                         table(updated_data$Table_Angle_errors)
                         updated_data$Table_Angle_errors_categorical<-ifelse(updated_data$`Table|_Angle`==updated_data$Table_Angle_errors,"No","Error")
                         table(updated_data$Table_Angle_errors_categorical)
                         
                         
                         
                         ##############treatment intent angle error#####################################
                         table(updated_data$Treatment_Intent)
                         updated_data$Treatment_Intent_errors<- (updated_data$Treatment_Intent)
                         library(plyr)
                         table(updated_data$Treatment_Intent_errors)
                         updated_data$Treatment_Intent_errors[sample(which(updated_data$Treatment_Intent_errors =="Curative"), 20)] <-"Treatment_Intent Should have been Palliative"
                         updated_data$Treatment_Intent_errors[sample(which(updated_data$Treatment_Intent_errors =="Palliative"), 40)] <-"Treatment_Intent Should have been Curative"
                         
                         table(updated_data$Treatment_Intent_errors)
                         updated_data$Treatment_Intent_errors_categorical<-ifelse(updated_data$Treatment_Intent==updated_data$Treatment_Intent_errors,"No","Error")
                         table(updated_data$Treatment_Intent_errors_categorical)
                         
                         
                         ############## mu per cgy error#####################################
                         table(updated_data$MU_cGy)
                         updated_data$MU_Per_cGy_errors<- (updated_data$MU_cGy)
                         library(plyr)
                         table(updated_data$MU_Per_cGy_errors)
                         updated_data$MU_Per_cGy_errors[sample(which(updated_data$MU_Per_cGy_errors ==0.06), 20)] <-"MU_cGy Should have been 4"
                         updated_data$MU_Per_cGy_errors[sample(which(updated_data$MU_Per_cGy_errors ==3.65), 30)] <-"MU_cGy Should have been 0.09"
                         
                         table(updated_data$MU_Per_cGy_errors)
                         updated_data$MU_Per_cGy_errors_categorical<-ifelse(updated_data$MU_cGy==updated_data$MU_Per_cGy_errors,"No","Error")
                         table(updated_data$MU_Per_cGy_errors_categorical)
                         
                         
                         
                         ############## mu per degree  error#####################################
                         table(updated_data$MU_deg)
                         updated_data$MU_Per_Degree_errors<- (updated_data$MU_deg)
                         library(plyr)
                         table(updated_data$MU_Per_Degree_errors)
                         updated_data$MU_Per_Degree_errors[sample(which(updated_data$MU_Per_Degree_errors ==0.01), 30)] <-"MU_deg Should have been 4"
                         updated_data$MU_Per_Degree_errors[sample(which(updated_data$MU_Per_Degree_errors ==1.4), 20)] <-"MU_deg Should have been 0.02"
                         # updated_data$MU_Per_Degree_errors[sample(which(updated_data$MU_Per_Degree_errors =='"[3.1-4]"'), 20)] <-'"[0.1-1]"'
                         
                         table(updated_data$MU_Per_Degree_errors)
                         updated_data$MU_Per_Degree_errors_categorical<-ifelse(updated_data$MU_deg==updated_data$MU_Per_Degree_errors,"No","Error")
                         table(updated_data$MU_Per_Degree_errors_categorical)
                         
                         
                         
                         
                         
                         ############## mu per degree  error#####################################
                         table(updated_data$PTV_Dose_Rx)
                         updated_data$PTV_Dose_Rx_errors<- (updated_data$PTV_Dose_Rx)
                         library(plyr)
                         table(updated_data$PTV_Dose_Rx_errors)
                         updated_data$PTV_Dose_Rx_errors[sample(which(updated_data$PTV_Dose_Rx_errors ==6000), 20)] <-"Wrong fractionation Should have been 6000"
                         updated_data$PTV_Dose_Rx_errors[sample(which(updated_data$PTV_Dose_Rx_errors ==4000), 20)] <-"Another combination PTV dose and fractions Should have been 6000"
                         updated_data$PTV_Dose_Rx_errors[sample(which(updated_data$PTV_Dose_Rx_errors ==4000), 20)] <-"Another combination of PTV_Dose and Number_of_Fractions Should have been 6000"
                         

                         table(updated_data$PTV_Dose_Rx_errors)
                         updated_data$PTV_Dose_Rx_errors_categorical<-ifelse(updated_data$PTV_Dose_Rx==updated_data$PTV_Dose_Rx_errors,"No","Error")
                         table(updated_data$PTV_Dose_Rx_errors_categorical)
                         
                         
                         #################################################
                         ##################################################
                         ##################################
                         updated_data$errors <- with(updated_data, ifelse(Bolus_errors_categorical=="Error" |
                                                                            Beam_Energy_errors_categorical == "Error" |
                                                                            Rx_Radiation_Type_errors_categorical=="Error"|
                                                                            Number_of_beams_errors_categorical=="Error"|
                                                                            SSD_errors_categorical=="Error"|
                                                                            CollimatorAngle_errors_categorical=="Error"|
                                                                            GantryAngle_errors_categorical=="Error"|
                                                                            Table_Angle_errors_categorical=="Error"|
                                                                            Treatment_Intent_errors_categorical=="Error"|
                                                                            MU_Per_cGy_errors_categorical=="Error"|
                                                                            MU_Per_Degree_errors_categorical=="Error"|
                                                                            PTV_Dose_Rx_errors_categorical=="Error",
                                                                          # PTV_Dose_Rx_errors_simulated_2_categorical=="Error"|
                                                                          # PTV_Dose_Rx_errors_simulated_3_categorical=="Error",
                                                                          1, 0))
                         
                         updated_data$errors_description<-paste(updated_data$Bolus_errors,
                                                                updated_data$Beam_Energy_errors,
                                                                updated_data$Rx_Radiation_Type_errors,
                                                                updated_data$Number_of_beams_errors,
                                                                updated_data$SSD_errors,
                                                                updated_data$CollimatorAngle_errors,
                                                                updated_data$GantryAngle_errors,
                                                                updated_data$Table_Angle_errors,
                                                                updated_data$Treatment_Intent_errors,
                                                                updated_data$MU_Per_cGy_errors,
                                                                updated_data$MU_Per_Degree_errors,
                                                                updated_data$PTV_Dose_Rx_errors, sep=",")                       
                         
                        
                         
                         ##################################################
                         ################################################
                         ############################################
                         ##################BINNING#######################
                         
                         #############################################
                         ####  Table Angle     #######################
                         #############################################
                         
                         #check the variable
                         names(updated_data)[names(updated_data) == "Table|_Angle"] <- "TableAngle"
                         
                         class(Data$TableAngle)
                         table(Data$TableAngle)
                         Data$TableAngle<- round(Data$TableAngle)
                         # 
                         # #check Luk's variable
                         # table(LukData$Table_Angle)
                         # summary(LukData$Table_Angle)
                         
                         library(tidyverse)
                         
                         
                         # 1-10, 11-20,21-30 until 90 and 91-269 and then 270-279,280-289...359 
                         
                         
                         
                         tags_TableAngle <- c( "[0-0]",
                                               "[1-10]",
                                               "[11-20]",
                                               "[21-30]",
                                               "[31-40]",
                                               "[41-50]",
                                               "[51-60]",
                                               "[61-70]", 
                                               "[71-80]",
                                               "[81-89]",
                                               "[90-90]",
                                               "[91-269]",
                                               "[270-270]",
                                               "[271-280]",
                                               "[281-290]",
                                               "[291-300]",
                                               "[301-310]",
                                               "[311-320]",
                                               "[321-330]",
                                               "[331-340]",
                                               "[341-350]",
                                               "[351-359]")
                         
                         
                         # 
                         TableAngle_bin <- updated_data %>% select(TableAngle) #pick the variable
                         vgroup <- as_tibble(TableAngle_bin) %>% 
                           
                           mutate(tag = case_when(
                             TableAngle < 1 ~tags_TableAngle[1],
                             TableAngle >= 1 & TableAngle <= 10 ~ tags_TableAngle[2],
                             TableAngle >= 11 & TableAngle <= 20 ~ tags_TableAngle[3],
                             TableAngle >= 21 & TableAngle <= 30 ~ tags_TableAngle[4],
                             TableAngle >= 31 & TableAngle <= 40 ~ tags_TableAngle[5],
                             TableAngle >= 41 & TableAngle <= 50 ~ tags_TableAngle[6],
                             TableAngle >= 51 & TableAngle <= 60 ~ tags_TableAngle[7],
                             TableAngle >= 61 & TableAngle <= 70 ~ tags_TableAngle[8],
                             TableAngle >= 71 & TableAngle <= 80 ~ tags_TableAngle[9],
                             TableAngle >= 81 & TableAngle < 90 ~ tags_TableAngle[10],
                             TableAngle == 90  ~ tags_TableAngle[11],
                             TableAngle >= 91 & TableAngle <= 269 ~ tags_TableAngle[12],
                             TableAngle ==270  ~ tags_TableAngle[13],
                             TableAngle >= 271 & TableAngle <= 280 ~ tags_TableAngle[14],
                             TableAngle >= 281 & TableAngle <= 290 ~ tags_TableAngle[15],
                             TableAngle >= 291 & TableAngle <= 300 ~ tags_TableAngle[16],
                             TableAngle >= 301 & TableAngle <= 310 ~ tags_TableAngle[17],
                             TableAngle >= 311 & TableAngle <= 320 ~ tags_TableAngle[18],
                             TableAngle >= 321 & TableAngle <= 330 ~ tags_TableAngle[19],
                             TableAngle >= 331 & TableAngle <= 340 ~ tags_TableAngle[20],
                             TableAngle >= 341 & TableAngle <= 350 ~ tags_TableAngle[21],
                             TableAngle >= 351 & TableAngle <= 359 ~ tags_TableAngle[22]))
                         
                         
                         
                         vgroup$tag_TableAngle <- factor(vgroup$tag,
                                                         levels = tags_TableAngle,
                                                         ordered = FALSE)
                         updated_data <- cbind(updated_data, bins_TableAngle = vgroup$tag_TableAngle)
                         
                         updated_data$bins_TableAngle<-as.character(updated_data$bins_TableAngle)
                         require(plyr)
                         require(dplyr)
                         revalue(updated_data$bins_TableAngle, c("[0-0]" = "0")) -> updated_data$bins_TableAngle
                         revalue(updated_data$bins_TableAngle, c("[90-90]" = "90")) -> updated_data$bins_TableAngle
                         revalue(updated_data$bins_TableAngle, c("[270-270]" = "90")) -> updated_data$bins_TableAngle
                         
                         table(updated_data$bins_TableAngle)
                         
                         
                         
                         
                         
                         
                         
                         #############################################
                         #### Control points##########################
                         #############################################
                         
                         #check the variable
                         class(updated_data$ControlPoints)
                         table(updated_data$ControlPoints)
                         
                         
                         #change it to numeric values 
                         updated_data$ControlPoints  <- as.numeric(as.character(updated_data$ControlPoints ))
                         
                         tags_ControlPoints <- c( "[1-1]",
                                                  "[2-20]",
                                                  "[21-40]",
                                                  "[41-60]",
                                                  "[61-80]",
                                                  "[81-100]",
                                                  "[101-120]",
                                                  "[121-140]", 
                                                  "[141-160]",
                                                  "[161-180]")
                         
                         ControlPoints_bin <- updated_data %>% select(ControlPoints) #pick the variable
                         vgroup <- as_tibble(ControlPoints_bin) %>% 
                           
                           mutate(tag = case_when(
                             ControlPoints <= 1 ~tags_ControlPoints[1],
                             ControlPoints > 2 & ControlPoints <= 20 ~ tags_ControlPoints[2],
                             ControlPoints >= 21 & ControlPoints <= 40 ~ tags_ControlPoints[3],
                             ControlPoints >= 41 & ControlPoints <= 60 ~ tags_ControlPoints[4],
                             ControlPoints >= 61 & ControlPoints <= 80 ~ tags_ControlPoints[5],
                             ControlPoints >= 81 & ControlPoints <= 100 ~ tags_ControlPoints[6],
                             ControlPoints >= 101 & ControlPoints <= 120 ~ tags_ControlPoints[7],
                             ControlPoints >= 121 & ControlPoints <= 140 ~ tags_ControlPoints[8],
                             ControlPoints >= 141 & ControlPoints <= 160 ~ tags_ControlPoints[9],
                             ControlPoints >= 161 & ControlPoints <= 180 ~ tags_ControlPoints[10],
                           ))
                         
                         
                         vgroup$tag_ControlPoints <- factor(vgroup$tag,
                                                            levels = tags_ControlPoints,
                                                            ordered = FALSE)
                         
                         
                         updated_data <- cbind(updated_data, bins_ControlPoints = vgroup$tag_ControlPoints)
                         
                         updated_data$bins_ControlPoints<-as.character(updated_data$bins_ControlPoints)
                         require(plyr)
                         require(dplyr)
                         revalue(updated_data$bins_ControlPoints, c("[1-1]" = "1")) -> updated_data$bins_ControlPoints
                         
                         
                         table(updated_data$bins_ControlPoints)
                         
                         
                         
                         
                         
                         
                         
                         
                         #############################################
                         #### Collimator angle #######################
                         #############################################
                         
                         #check the collimator angle
                         table(updated_data$CollimatorAngle)
                         #round the values
                         updated_data$CollimatorAngle <- round(updated_data$CollimatorAngle) # Round off the column t
                         
                         tags_CollimatorAngle <- c( "[0-0]",
                                                    "[1-10]",
                                                    "[11-20]",
                                                    "[21-30]",
                                                    "[31-40]",
                                                    "[41-50]",
                                                    "[51-60]",
                                                    "[61-70]", 
                                                    "[71-80]",
                                                    "[81-89]",
                                                    "[90-90]",
                                                    "[91-100]",
                                                    "[101-110]",
                                                    "[111-120]",
                                                    "[121-130]",
                                                    "[131-140]",
                                                    "[141-150]",
                                                    "[151-160]",
                                                    "[161-170]",
                                                    "[171-179]",
                                                    "[180-180]",
                                                    "[181-190]",
                                                    "[191-200]",
                                                    "[201-210]",
                                                    "[211-220]",
                                                    "[221-230]",
                                                    "[231-240]",
                                                    "[241-250]", 
                                                    "[251-260]",
                                                    "[261-269]",
                                                    "[270-270]",
                                                    "[271-280]",
                                                    "[281-290]",
                                                    "[291-300]",
                                                    "[301-310]",
                                                    "[311-320]",
                                                    "[321-330]",
                                                    "[331-340]",
                                                    "[341-350]",
                                                    "[351-359]",
                                                    "[360-360]")
                         
                         
                         
                         
                         CollimatorAngle_bin <- updated_data %>% select(CollimatorAngle ) #pick the variable
                         vgroup <- as_tibble(CollimatorAngle_bin) %>% 
                           
                           
                           
                           mutate(tag = case_when(
                             CollimatorAngle < 1 ~tags_CollimatorAngle[1],
                             CollimatorAngle >= 1 & CollimatorAngle <= 10 ~ tags_CollimatorAngle[2],
                             CollimatorAngle >= 11 & CollimatorAngle <= 20 ~ tags_CollimatorAngle[3],
                             CollimatorAngle >= 21 & CollimatorAngle <= 30 ~ tags_CollimatorAngle[4],
                             CollimatorAngle >= 31 & CollimatorAngle <= 40 ~ tags_CollimatorAngle[5],
                             CollimatorAngle >= 41 & CollimatorAngle <= 50 ~ tags_CollimatorAngle[6],
                             CollimatorAngle >= 51 & CollimatorAngle <= 60 ~ tags_CollimatorAngle[7],
                             CollimatorAngle >= 61 & CollimatorAngle <= 70 ~ tags_CollimatorAngle[8],
                             CollimatorAngle >= 71 & CollimatorAngle <= 80 ~ tags_CollimatorAngle[9],
                             CollimatorAngle >= 81 & CollimatorAngle <= 89 ~ tags_CollimatorAngle[10],
                             CollimatorAngle == 90 ~ tags_CollimatorAngle[11],
                             CollimatorAngle >= 91 & CollimatorAngle <= 100 ~ tags_CollimatorAngle[12],
                             CollimatorAngle >= 101 & CollimatorAngle <= 110 ~ tags_CollimatorAngle[13],
                             CollimatorAngle >= 111 & CollimatorAngle <= 120 ~ tags_CollimatorAngle[14],
                             CollimatorAngle >= 121 & CollimatorAngle <= 130 ~ tags_CollimatorAngle[15],
                             CollimatorAngle >= 131 & CollimatorAngle <= 140 ~ tags_CollimatorAngle[16],
                             CollimatorAngle >= 141 & CollimatorAngle <= 150 ~ tags_CollimatorAngle[17],
                             CollimatorAngle >= 151 & CollimatorAngle <= 160 ~ tags_CollimatorAngle[18],
                             CollimatorAngle >= 161 & CollimatorAngle <= 170 ~ tags_CollimatorAngle[19],
                             CollimatorAngle >= 171 & CollimatorAngle <= 179 ~ tags_CollimatorAngle[20],
                             CollimatorAngle == 180 ~ tags_CollimatorAngle[21],
                             CollimatorAngle >= 181 & CollimatorAngle <= 190 ~ tags_CollimatorAngle[22],
                             CollimatorAngle >= 191 & CollimatorAngle <= 200 ~ tags_CollimatorAngle[23],
                             CollimatorAngle >= 201 & CollimatorAngle <= 210 ~ tags_CollimatorAngle[24],
                             CollimatorAngle >= 211 & CollimatorAngle <= 220 ~ tags_CollimatorAngle[25],
                             CollimatorAngle >= 221 & CollimatorAngle <= 230 ~ tags_CollimatorAngle[26],
                             CollimatorAngle >= 231 & CollimatorAngle <= 240 ~ tags_CollimatorAngle[27],
                             CollimatorAngle >= 241 & CollimatorAngle <= 250 ~ tags_CollimatorAngle[28],
                             CollimatorAngle >= 251 & CollimatorAngle <= 260 ~ tags_CollimatorAngle[29],
                             CollimatorAngle >= 261 & CollimatorAngle <= 269 ~ tags_CollimatorAngle[30],
                             CollimatorAngle == 270  ~ tags_CollimatorAngle[31],
                             CollimatorAngle >= 271 & CollimatorAngle <= 280 ~ tags_CollimatorAngle[32],
                             CollimatorAngle >= 281 & CollimatorAngle <= 290 ~ tags_CollimatorAngle[33],
                             CollimatorAngle >= 291 & CollimatorAngle <= 300 ~ tags_CollimatorAngle[34],
                             CollimatorAngle >= 301 & CollimatorAngle <= 310 ~ tags_CollimatorAngle[35],
                             CollimatorAngle >= 311 & CollimatorAngle <= 320 ~ tags_CollimatorAngle[36],
                             CollimatorAngle >= 321 & CollimatorAngle <= 330 ~ tags_CollimatorAngle[37],
                             CollimatorAngle >= 331 & CollimatorAngle <= 340 ~ tags_CollimatorAngle[38],
                             CollimatorAngle >= 341 & CollimatorAngle <= 350 ~ tags_CollimatorAngle[39],
                             CollimatorAngle >= 351 & CollimatorAngle <= 359 ~ tags_CollimatorAngle[40],
                             CollimatorAngle == 360 ~ tags_CollimatorAngle[41]))
                         
                         
                         vgroup$tag_CollimatorAngle <- factor(vgroup$tag,
                                                              levels = tags_CollimatorAngle,
                                                              ordered = FALSE)
                         
                         
                         updated_data <- cbind(updated_data, bins_CollimatorAngle = vgroup$tag_CollimatorAngle)
                         
                         updated_data$bins_CollimatorAngle<-as.character(updated_data$bins_CollimatorAngle)
                         require(plyr)
                         require(dplyr)
                         revalue(updated_data$bins_CollimatorAngle, c("[0-0]" = "0")) -> updated_data$bins_CollimatorAngle
                         revalue(updated_data$bins_CollimatorAngle, c("[90-90]" = "90")) -> updated_data$bins_CollimatorAngle
                         revalue(updated_data$bins_CollimatorAngle, c("[270-270]" = "270")) -> updated_data$bins_CollimatorAngle
                         revalue(updated_data$bins_CollimatorAngle, c("[360-360]" = "360")) -> updated_data$bins_CollimatorAngle
                         
                         table(updated_data$bins_CollimatorAngle)
                         
                         
                         
                         
                         
                         
                         
                         
                         #############################################
                         ####   Gantry Angle #########################
                         #############################################
                         table(updated_data$GantryAngle)
                         updated_data$GantryAngle<- round(updated_data$GantryAngle)
                         
                         
                         tags_GantryAngle <- c( "[0-0]",
                                                "[1-10]",
                                                "[11-20]",
                                                "[21-30]",
                                                "[31-40]",
                                                "[41-50]",
                                                "[51-60]",
                                                "[61-70]", 
                                                "[71-80]",
                                                "[81-89]",
                                                "[90-90]",
                                                "[91-100]",
                                                "[101-110]",
                                                "[111-120]",
                                                "[121-130]",
                                                "[131-140]",
                                                "[141-150]",
                                                "[151-160]",
                                                "[161-170]",
                                                "[171-179]",
                                                "[180-180]",
                                                "[181-190]",
                                                "[191-200]",
                                                "[201-210]",
                                                "[211-220]",
                                                "[221-230]",
                                                "[231-240]",
                                                "[241-250]", 
                                                "[251-260]",
                                                "[261-269]",
                                                "[270-270]",
                                                "[271-280]",
                                                "[281-290]",
                                                "[291-300]",
                                                "[301-310]",
                                                "[311-320]",
                                                "[321-330]",
                                                "[331-340]",
                                                "[341-350]",
                                                "[351-359]",
                                                "[360-360]")
                         
                         
                         GantryAngle_bin <- updated_data %>% select(GantryAngle ) #pick the variable
                         vgroup <- as_tibble(GantryAngle_bin) %>% 
                           
                           
                           
                           mutate(tag = case_when(
                             GantryAngle < 1 ~tags_GantryAngle[1],
                             GantryAngle >= 1 & GantryAngle <= 10 ~ tags_GantryAngle[2],
                             GantryAngle >= 11 & GantryAngle <= 20 ~ tags_GantryAngle[3],
                             GantryAngle >= 21 & GantryAngle <= 30 ~ tags_GantryAngle[4],
                             GantryAngle >= 31 & GantryAngle <= 40 ~ tags_GantryAngle[5],
                             GantryAngle >= 41 & GantryAngle <= 50 ~ tags_GantryAngle[6],
                             GantryAngle >= 51 & GantryAngle <= 60 ~ tags_GantryAngle[7],
                             GantryAngle >= 61 & GantryAngle <= 70 ~ tags_GantryAngle[8],
                             GantryAngle >= 71 & GantryAngle <= 80 ~ tags_GantryAngle[9],
                             GantryAngle >= 81 & GantryAngle <= 89 ~ tags_GantryAngle[10],
                             GantryAngle ==90 ~ tags_GantryAngle[11],
                             GantryAngle >= 91 & GantryAngle <= 100 ~ tags_GantryAngle[12],
                             GantryAngle >= 101 & GantryAngle <= 110 ~ tags_GantryAngle[13],
                             GantryAngle >= 111 & GantryAngle <= 120 ~ tags_GantryAngle[14],
                             GantryAngle >= 121 & GantryAngle <= 130 ~ tags_GantryAngle[15],
                             GantryAngle >= 131 & GantryAngle <= 140 ~ tags_GantryAngle[16],
                             GantryAngle >= 141 & GantryAngle <= 150 ~ tags_GantryAngle[17],
                             GantryAngle >= 151 & GantryAngle <= 160 ~ tags_GantryAngle[18],
                             GantryAngle >= 161 & GantryAngle <= 170 ~ tags_GantryAngle[19],
                             GantryAngle >= 171 & GantryAngle <= 179 ~ tags_GantryAngle[20],
                             GantryAngle ==180 ~ tags_GantryAngle[21],
                             GantryAngle >= 181 & GantryAngle <= 190 ~ tags_GantryAngle[21],
                             GantryAngle >= 191 & GantryAngle <= 200 ~ tags_GantryAngle[22],
                             GantryAngle >= 201 & GantryAngle <= 210 ~ tags_GantryAngle[23],
                             GantryAngle >= 211 & GantryAngle <= 220 ~ tags_GantryAngle[24],
                             GantryAngle >= 221 & GantryAngle <= 230 ~ tags_GantryAngle[25],
                             GantryAngle >= 231 & GantryAngle <= 240 ~ tags_GantryAngle[26],
                             GantryAngle >= 241 & GantryAngle <= 250 ~ tags_GantryAngle[27],
                             GantryAngle >= 251 & GantryAngle <= 260 ~ tags_GantryAngle[28],
                             GantryAngle >= 261 & GantryAngle <= 269 ~ tags_GantryAngle[29],
                             GantryAngle ==270 ~ tags_GantryAngle[30],
                             GantryAngle >= 271 & GantryAngle <= 280 ~ tags_GantryAngle[31],
                             GantryAngle >= 281 & GantryAngle <= 290 ~ tags_GantryAngle[32],
                             GantryAngle >= 291 & GantryAngle <= 300 ~ tags_GantryAngle[33],
                             GantryAngle >= 301 & GantryAngle <= 310 ~ tags_GantryAngle[34],
                             GantryAngle >= 311 & GantryAngle <= 320 ~ tags_GantryAngle[35],
                             GantryAngle >= 321 & GantryAngle <= 330 ~ tags_GantryAngle[36],
                             GantryAngle >= 331 & GantryAngle <= 340 ~ tags_GantryAngle[37],
                             GantryAngle >= 341 & GantryAngle <= 350 ~ tags_GantryAngle[38],
                             GantryAngle >= 351 & GantryAngle <= 359 ~ tags_GantryAngle[39],
                             GantryAngle == 360 ~ tags_GantryAngle[40]
                           ))
                         
                         
                         vgroup$tag_GantryAngle <- factor(vgroup$tag,
                                                          levels = tags_GantryAngle,
                                                          ordered = FALSE)
                         
                         updated_data <- cbind(updated_data, bins_GantryAngle = vgroup$tag_GantryAngle)
                         
                         updated_data$bins_GantryAngle<-as.character(updated_data$bins_GantryAngle)
                         require(plyr)
                         require(dplyr)
                         revalue(updated_data$bins_GantryAngle, c("[0-0]" = "0")) -> updated_data$bins_GantryAngle
                         revalue(updated_data$bins_GantryAngle, c("[90-90]" = "90")) -> updated_data$bins_GantryAngle
                         revalue(updated_data$bins_GantryAngle, c("[180-180]" = "180")) -> updated_data$bins_GantryAngle
                         revalue(updated_data$bins_GantryAngle, c("[270-270]" = "270")) -> updated_data$bins_GantryAngle
                         
                         table(updated_data$bins_GantryAngle)
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         # #############################################
                         # ####  MU_deg    ###############################
                         # #############################################
                         table(updated_data$MU_deg)
                         
                         updated_data$MU_deg <- as.numeric(as.character(updated_data$MU_deg ))
                         tags_MU_deg <- c( "[0-0]",
                                           "[0.1-1]",
                                           "[1-1.2]",
                                           "[2.1-3]",
                                           "[3.1-4]",
                                           "[4.1-5]",
                                           "[5.1-20]")
                         MU_deg_bin <- updated_data %>% select(MU_deg) #pick the variable
                         vgroup <- as_tibble(MU_deg_bin) %>%
                           
                           mutate(tag = case_when(
                             MU_deg == 0 ~tags_MU_deg[1],
                             MU_deg > 0 & MU_deg <= 1 ~ tags_MU_deg[2],
                             MU_deg >= 1.1 & MU_deg <=2  ~ tags_MU_deg[3],
                             MU_deg >= 2.1 & MU_deg <= 3 ~ tags_MU_deg[4],
                             MU_deg >= 3.1 & MU_deg <= 4~ tags_MU_deg[5],
                             MU_deg >= 4.1 & MU_deg <= 5 ~ tags_MU_deg[6],
                             MU_deg >=5 ~ tags_MU_deg[7]
                           ))
                         #
                         #
                         #
                         vgroup$tag_MU_deg <- factor(vgroup$tag,
                                                     levels = tags_MU_deg,
                                                     ordered = FALSE)
                         updated_data <- cbind(updated_data, bins_MU_deg = vgroup$tag_MU_deg)
                         
                         updated_data$bins_MU_deg<-as.character(updated_data$bins_MU_deg)
                         require(plyr)
                         require(dplyr)
                         revalue(updated_data$bins_MU_deg, c("[0-0]" = "0")) -> updated_data$bins_MU_deg
                         revalue(updated_data$bins_MU_deg, c("[5.1-20]" = "more than 5")) -> updated_data$bins_MU_deg
                         
                         #
                         table(updated_data$bins_MU_deg)
                         
                         
                         
                         # #############################################
                         # ####  MU_cGy     ###############################
                         # #############################################
                         #
                         #check variable
                         table(updated_data$MU_cGy)
                         class(updated_data$MU_cGy)
                         updated_data$MU_cGy<- round(updated_data$MU_cGy, digits = 2)
                         #
                         updated_data$MU_cGy <- as.numeric(as.character(updated_data$MU_cGy ))
                         
                         
                         
                         tags_MU_cGy <- c( "[0-1]",
                                           "[1.1-1.5]",
                                           "[1.6-2]",
                                           "[2.1-2.5]",
                                           "[2.6-3]",
                                           "[3.1-3.5]",
                                           "[3.6-4]", 
                                           "[4.1-4.5]",
                                           "[4.6-5]",
                                           "[5.1-7]")
                         
                         MU_cGy_bin <- updated_data %>% select(MU_cGy) #pick the variable
                         vgroup <- as_tibble(MU_cGy_bin) %>%
                           
                           mutate(tag = case_when(
                             MU_cGy >= 0 & MU_cGy <= 1 ~ tags_MU_cGy[1],
                             MU_cGy >= 1.1 & MU_cGy <= 1.5 ~ tags_MU_cGy[2],
                             MU_cGy >= 1.6 & MU_cGy <= 2 ~ tags_MU_cGy[3],
                             MU_cGy >= 2.1 & MU_cGy <= 2.5 ~ tags_MU_cGy[4],
                             MU_cGy >= 2.6 & MU_cGy <= 3 ~ tags_MU_cGy[5],
                             MU_cGy >= 3.1 & MU_cGy <= 3.5 ~ tags_MU_cGy[6],
                             MU_cGy >= 3.6 & MU_cGy <= 4 ~ tags_MU_cGy[7],
                             MU_cGy >= 4.1 & MU_cGy <= 4.5 ~ tags_MU_cGy[8],
                             MU_cGy >= 4.6 & MU_cGy <= 5 ~ tags_MU_cGy[9],
                             MU_cGy >= 5.1 & MU_cGy <= 7 ~ tags_MU_cGy[10]))
                         
                         
                         vgroup$tag_MU_cGy<- factor(vgroup$tag,
                                                    levels = tags_MU_cGy,
                                                    ordered = FALSE)
                         
                         updated_data <- cbind(updated_data, bins_MU_cGy = vgroup$tag_MU_cGy)
                         table(updated_data$bins_MU_cGy)
                         
                         updated_data$bins_MU_cGy<-as.character(updated_data$bins_MU_cGy)
                         require(plyr)
                         require(dplyr)
                         revalue(updated_data$bins_MU_cGy, c("[5.1-7]" = "more than 5")) -> updated_data$bins_MU_cGy
                         table(updated_data$bins_MU_cGy)
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         ###############
                         ###ssd THE CORRECT ONE FOR THE SSD
                         ########
                         table(updated_data$SSD)
                         updated_data$SSD<- round(updated_data$SSD)
                         
                         
                         
                         updated_data$SSD <- as.numeric(as.character(updated_data$SSD ))
                         
                         
                         
                         tags_SSD <- c( "[0-79]",
                                        "[80-85]",
                                        "[86-90]",
                                        "[91-95]",
                                        "[96-100]",
                                        "[101-105]",
                                        "[106-110]",
                                        "[111-115]", 
                                        "[116-120]",
                                        "[121-366]")
                         
                         SSD_bin <- updated_data %>% select(SSD) #pick the variable
                         vgroup <- as_tibble(SSD_bin) %>%
                           
                           mutate(tag = case_when(
                             SSD <= 79 ~ tags_SSD[1],
                             SSD >= 80 & SSD <= 85 ~ tags_SSD[2],
                             SSD >= 86 & SSD <= 90 ~ tags_SSD[3],
                             SSD >= 91 & SSD <= 95 ~ tags_SSD[4],
                             SSD >= 96 & SSD <= 100 ~ tags_SSD[5],
                             SSD >= 101 & SSD <= 105 ~ tags_SSD[6],
                             SSD >= 106 & SSD <= 110 ~ tags_SSD[7],
                             SSD >= 111 & SSD <= 115 ~ tags_SSD[8],
                             SSD >= 116 & SSD <= 120 ~ tags_SSD[9],
                             SSD >= 121  ~ tags_SSD[10]))
                         
                         
                         vgroup$tag_SSD<- factor(vgroup$tag,
                                                 levels = tags_SSD,
                                                 ordered = FALSE)
                         
                         updated_data <- cbind(updated_data, bins_SSD = vgroup$tag_SSD)
                         table(updated_data$bins_SSD)
                         
                         updated_data$bins_SSD<-as.character(updated_data$bins_SSD)
                         require(plyr)
                         require(dplyr)
                         revalue(updated_data$bins_SSD, c("[0-79]" = "less than 80")) -> updated_data$bins_SSD
                         revalue(updated_data$bins_SSD, c("[121-366]" = "more than 120")) -> updated_data$bins_SSD
                         table(updated_data$bins_SSD)
                         
                         
                         
                         
                         
                         
                         ####binning dose per fraction####
                         
                         
                         
                         
                         # DataFinal$Dose_Per_Fraction <- as.character(as.factor(DataFinal$Dose_Per_Fraction))
                         
                         
                         
                         updated_data$Dose_Per_Fraction <- as.numeric(as.character(updated_data$Dose_Per_Fraction ))
                         
                         
                         
                         tags_Dose_Per_Fraction <- c( "[0-150]",
                                                      "[151-200]",
                                                      "[201-250]",
                                                      "[251-300]",
                                                      "[301-350]",
                                                      "[351-400]",
                                                      "[401-600]",
                                                      "[601-800]",
                                                      "[801-1000]",
                                                      "[1001-1200]",
                                                      "[1201-1400]",
                                                      "[1401-1600]",
                                                      "[1601-1800]",
                                                      "[1801-2000]",
                                                      "[2001-2500]",
                                                      "[2501-3000]",
                                                      "[3001-5000]")
                         
                         Dose_Per_Fraction_bin <- updated_data %>% select(Dose_Per_Fraction) #pick the variable
                         vgroup <- as_tibble(Dose_Per_Fraction_bin) %>%
                           
                           mutate(tag = case_when(
                             Dose_Per_Fraction >= 0 & Dose_Per_Fraction <= 150 ~ tags_Dose_Per_Fraction[1],
                             Dose_Per_Fraction >= 151 & Dose_Per_Fraction <= 200 ~ tags_Dose_Per_Fraction[2],
                             Dose_Per_Fraction >= 201 & Dose_Per_Fraction <= 250 ~ tags_Dose_Per_Fraction[3],
                             Dose_Per_Fraction >= 251 & Dose_Per_Fraction <= 300 ~ tags_Dose_Per_Fraction[4],
                             Dose_Per_Fraction >= 301 & Dose_Per_Fraction <= 350 ~ tags_Dose_Per_Fraction[5],
                             Dose_Per_Fraction >= 351 & Dose_Per_Fraction <= 400 ~ tags_Dose_Per_Fraction[6],
                             Dose_Per_Fraction >= 401 & Dose_Per_Fraction <= 600 ~ tags_Dose_Per_Fraction[7],
                             Dose_Per_Fraction >= 601 & Dose_Per_Fraction <= 800 ~ tags_Dose_Per_Fraction[8],
                             Dose_Per_Fraction >= 801 & Dose_Per_Fraction <= 1000 ~ tags_Dose_Per_Fraction[9],
                             Dose_Per_Fraction >= 1001 & Dose_Per_Fraction <= 1200 ~ tags_Dose_Per_Fraction[10],
                             Dose_Per_Fraction >= 1201 & Dose_Per_Fraction <= 1400 ~ tags_Dose_Per_Fraction[11],
                             Dose_Per_Fraction >= 1401 & Dose_Per_Fraction <= 1600 ~ tags_Dose_Per_Fraction[12],
                             Dose_Per_Fraction >= 1601 & Dose_Per_Fraction <= 1800 ~ tags_Dose_Per_Fraction[13],
                             Dose_Per_Fraction >= 1801 & Dose_Per_Fraction <= 2000 ~ tags_Dose_Per_Fraction[14],
                             Dose_Per_Fraction >= 2001 & Dose_Per_Fraction <= 2500 ~ tags_Dose_Per_Fraction[15],
                             Dose_Per_Fraction >= 2501 & Dose_Per_Fraction <= 3000 ~ tags_Dose_Per_Fraction[16],
                             Dose_Per_Fraction >= 3001 & Dose_Per_Fraction <= 5000 ~ tags_Dose_Per_Fraction[17]))
                         
                         
                         vgroup$tag_Dose_Per_Fraction<- factor(vgroup$tag,
                                                               levels = tags_Dose_Per_Fraction,
                                                               ordered = FALSE)
                         
                         updated_data <- cbind(updated_data, bins_Dose_Per_Fraction = vgroup$tag_Dose_Per_Fraction)
                         table(updated_data$bins_Dose_Per_Fraction)
                         
                         updated_data$bins_Dose_Per_Fraction<-as.character(updated_data$bins_Dose_Per_Fraction)
                         require(plyr)
                         require(dplyr)
                         revalue(updated_data$bins_Dose_Per_Fraction, c("[0-150]" = "less or equal than 150")) -> updated_data$bins_Dose_Per_Fraction
                         revalue(updated_data$bins_Dose_Per_Fraction, c("[3001-5000]" = "more than 3000")) -> updated_data$bins_Dose_Per_Fraction
                         table(updated_data$bins_Dose_Per_Fraction)
                         
                         
                         
                         
                         ####PTV dose####
                         
                         
                         
                         
                         # DataFinal$Dose_Per_Fraction <- as.character(as.factor(DataFinal$Dose_Per_Fraction))
                         
                         
                         
                         updated_data$PTV_Dose_Rx <- as.numeric(as.character(updated_data$PTV_Dose_Rx ))
                         
                         table(updated_data$PTV_Dose_Rx)
                         
                         tags_PTV_Dose_Rx <- c( "[0-200]",
                                                "[201-400]",
                                                "[401-600]",
                                                "[601-800]",
                                                "[801-1000]",
                                                "[1001-1200]",
                                                "[1201-1400]",
                                                "[1401-1600]",
                                                "[1601-1800]",
                                                "[1801-2000]",
                                                "[2001-2200]",
                                                "[2201-2400]",
                                                "[2401-2600]",
                                                "[2601-2800]",
                                                "[2801-3000]",
                                                "[3001-3200]",
                                                "[3201-3400]",
                                                "[3401-3600]",
                                                "[3601-3800]",
                                                "[3801-4000]",
                                                "[4001-4200]",
                                                "[4201-4400]",
                                                "[4401-4600]",
                                                "[4601-4800]",
                                                "[4801-5000]",
                                                "[5001-5200]",
                                                "[5201-5400]",
                                                "[5401-5600]",
                                                "[5601-5800]",
                                                "[5801-6000]",
                                                "[6001-6200]",
                                                "[6201-6400]",
                                                "[6401-6600]",
                                                "[6601-6800]",
                                                "[6801-7000]",
                                                "[7001-7200]",
                                                "[7201-7400]",
                                                "[7401-7600]",
                                                "[7601-7800]",
                                                "[7801-8000]",
                                                "[8001-20000]")
                         
                         PTV_Dose_Rx_bin <- updated_data %>% select(PTV_Dose_Rx) #pick the variable
                         vgroup <- as_tibble(PTV_Dose_Rx_bin) %>%
                           
                           mutate(tag = case_when(
                             PTV_Dose_Rx >= 0 & PTV_Dose_Rx <= 200 ~ tags_PTV_Dose_Rx[1],
                             PTV_Dose_Rx >= 201 & PTV_Dose_Rx <= 400 ~ tags_PTV_Dose_Rx[2],
                             PTV_Dose_Rx >= 401 & PTV_Dose_Rx <= 600 ~ tags_PTV_Dose_Rx[3],
                             PTV_Dose_Rx >= 601 & PTV_Dose_Rx <= 800 ~ tags_PTV_Dose_Rx[4],
                             PTV_Dose_Rx >= 801 & PTV_Dose_Rx <= 1000 ~ tags_PTV_Dose_Rx[5],
                             PTV_Dose_Rx >= 1001 & PTV_Dose_Rx <= 1200 ~ tags_PTV_Dose_Rx[6],
                             PTV_Dose_Rx >= 1201 & PTV_Dose_Rx <= 1400 ~ tags_PTV_Dose_Rx[7],
                             PTV_Dose_Rx >= 1401 & PTV_Dose_Rx <= 1600 ~ tags_PTV_Dose_Rx[8],
                             PTV_Dose_Rx >= 1601 & PTV_Dose_Rx <= 1800 ~ tags_PTV_Dose_Rx[9],
                             PTV_Dose_Rx >= 1801 & PTV_Dose_Rx <= 2000 ~ tags_PTV_Dose_Rx[10],
                             PTV_Dose_Rx >= 2001 & PTV_Dose_Rx <= 2200 ~ tags_PTV_Dose_Rx[11],
                             PTV_Dose_Rx >= 2201 & PTV_Dose_Rx <= 2400 ~ tags_PTV_Dose_Rx[12],
                             PTV_Dose_Rx >= 2401 & PTV_Dose_Rx <= 2600 ~ tags_PTV_Dose_Rx[13],
                             PTV_Dose_Rx >= 2601 & PTV_Dose_Rx <= 2800 ~ tags_PTV_Dose_Rx[14],
                             PTV_Dose_Rx >= 2801 & PTV_Dose_Rx <= 3000 ~ tags_PTV_Dose_Rx[15],
                             PTV_Dose_Rx >= 3001 & PTV_Dose_Rx <= 3200 ~ tags_PTV_Dose_Rx[16],
                             PTV_Dose_Rx >= 3201 & PTV_Dose_Rx <= 3400 ~ tags_PTV_Dose_Rx[17],
                             PTV_Dose_Rx >= 3401 & PTV_Dose_Rx <= 3600 ~ tags_PTV_Dose_Rx[18],
                             PTV_Dose_Rx >= 3601 & PTV_Dose_Rx <= 3800 ~ tags_PTV_Dose_Rx[19],
                             PTV_Dose_Rx >= 3801 & PTV_Dose_Rx <= 4000 ~ tags_PTV_Dose_Rx[20],
                             PTV_Dose_Rx >= 4001 & PTV_Dose_Rx <= 4200 ~ tags_PTV_Dose_Rx[21],
                             PTV_Dose_Rx >= 4201 & PTV_Dose_Rx <= 4400 ~ tags_PTV_Dose_Rx[22],
                             PTV_Dose_Rx >= 4401 & PTV_Dose_Rx <= 4600 ~ tags_PTV_Dose_Rx[23],
                             PTV_Dose_Rx >= 4601 & PTV_Dose_Rx <= 4800 ~ tags_PTV_Dose_Rx[24],
                             PTV_Dose_Rx >= 4801 & PTV_Dose_Rx <= 5000 ~ tags_PTV_Dose_Rx[25],
                             PTV_Dose_Rx >= 5001 & PTV_Dose_Rx <= 5200 ~ tags_PTV_Dose_Rx[26],
                             PTV_Dose_Rx >= 5201 & PTV_Dose_Rx <= 5400 ~ tags_PTV_Dose_Rx[27],
                             PTV_Dose_Rx >= 5401 & PTV_Dose_Rx <= 5600 ~ tags_PTV_Dose_Rx[28],
                             PTV_Dose_Rx >= 5601 & PTV_Dose_Rx <= 5800 ~ tags_PTV_Dose_Rx[29],
                             PTV_Dose_Rx >= 5801 & PTV_Dose_Rx <= 6000 ~ tags_PTV_Dose_Rx[30],
                             PTV_Dose_Rx >= 6001 & PTV_Dose_Rx <= 6200 ~ tags_PTV_Dose_Rx[31],
                             PTV_Dose_Rx >= 6201 & PTV_Dose_Rx <= 6400 ~ tags_PTV_Dose_Rx[32],
                             PTV_Dose_Rx >= 6401 & PTV_Dose_Rx <= 6600 ~ tags_PTV_Dose_Rx[33],
                             PTV_Dose_Rx >= 6601 & PTV_Dose_Rx <= 6800 ~ tags_PTV_Dose_Rx[34],
                             PTV_Dose_Rx >= 6801 & PTV_Dose_Rx <= 7000 ~ tags_PTV_Dose_Rx[35],
                             PTV_Dose_Rx >= 7001 & PTV_Dose_Rx <= 7200 ~ tags_PTV_Dose_Rx[36],
                             PTV_Dose_Rx >= 7201 & PTV_Dose_Rx <= 7400 ~ tags_PTV_Dose_Rx[37],
                             PTV_Dose_Rx >= 7401 & PTV_Dose_Rx <= 7600 ~ tags_PTV_Dose_Rx[38],
                             PTV_Dose_Rx >= 7601 & PTV_Dose_Rx <= 7800 ~ tags_PTV_Dose_Rx[39],
                             PTV_Dose_Rx >= 7801 & PTV_Dose_Rx <= 8000 ~ tags_PTV_Dose_Rx[40],
                             PTV_Dose_Rx >= 8001 & PTV_Dose_Rx <= 20000 ~ tags_PTV_Dose_Rx[41]))
                         
                         
                         vgroup$tag_PTV_Dose_Rx<- factor(vgroup$tag,
                                                         levels = tags_PTV_Dose_Rx,
                                                         ordered = FALSE)
                         
                         updated_data <- cbind(updated_data, bins_PTV_Dose_Rx = vgroup$tag_PTV_Dose_Rx)
                         table(updated_data$bins_PTV_Dose_Rx)
                         
                         updated_data$bins_PTV_Dose_Rx<-as.character(updated_data$bins_PTV_Dose_Rx)
                         require(plyr)
                         require(dplyr)
                         # revalue(DataFinal$bins_PTV_Dose_Rx, c("[0-150]" = "less or equal than 150")) -> DataFinal$bins_PTV_Dose_Rx
                         revalue(updated_data$bins_PTV_Dose_Rx, c("[8001-20000]" = "more than 8000")) -> updated_data$bins_PTV_Dose_Rx
                         table(updated_data$bins_PTV_Dose_Rx)
                         
                         
                         
                         
                         
                         
                         ##### put quatation marks
                         updated_data$Researchnumber<- paste0('"', updated_data$Researchnumber, '"')
                         
                         
                         
                         #remove unnecessary columns
                         updated_data$MU_cGy<-NULL
                         updated_data$MU_deg<-NULL
                         updated_data$TableAngle<-NULL
                         updated_data$ControlPoints<-NULL
                         updated_data$SSD<-NULL
                         updated_data$GantryAngle<-NULL
                         updated_data$GantryDirection<-NULL
                         updated_data$GantryStopAngle<-NULL
                         updated_data$CollimatorAngle<-NULL
                         updated_data$Mu<-NULL
                         
                         
                         
                         colnames(updated_data)
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         #put quation marks
                         
                         updated_data$Anatomic_tumour_loc<- paste0('"', updated_data$Anatomic_tumour_loc, '"')
                         updated_data$T_stage<- paste0('"', updated_data$T_stage, '"')
                         updated_data$N_stage<- paste0('"', updated_data$N_stage, '"')
                         updated_data$M_stage<- paste0('"', updated_data$M_stage, '"')
                         updated_data$Plan_Technique<- paste0('"', updated_data$Plan_Technique, '"')
                         table(updated_data$Wedge)
                         class(updated_data$Wedge)
                         updated_data$Wedge <- as.character(as.numeric(updated_data$Wedge))
                         revalue(updated_data$Wedge, c("0" = "No")) -> updated_data$Wedge
                         updated_data$Wedge<- paste0('"', updated_data$Wedge, '"')
                         updated_data$Bolus<- paste0('"', updated_data$Bolus, '"')
                         updated_data$Orientation<- paste0('"', updated_data$Orientation, '"')
                         updated_data$Tolerance<- paste0('"', updated_data$Tolerance, '"')
                         updated_data$Treatment_Intent<- paste0('"', updated_data$Treatment_Intent, '"')
                         updated_data$Rx_Radiation_Type<- paste0('"', updated_data$Rx_Radiation_Type, '"')
                         updated_data$bins_TableAngle<- paste0('"', updated_data$bins_TableAngle, '"')
                         names(updated_data)[names(updated_data) == "bins_TableAngle"] <- "Table_Angle"
                         updated_data$bins_CollimatorAngle<- paste0('"', updated_data$bins_CollimatorAngle, '"')
                         names(updated_data)[names(updated_data) == "bins_CollimatorAngle"] <- "Collimator_Angle"
                         updated_data$bins_ControlPoints<- paste0('"', updated_data$bins_ControlPoints, '"')
                         names(updated_data)[names(updated_data) == "bins_ControlPoints"] <- "Control_Points"
                         updated_data$bins_GantryAngle<- paste0('"', updated_data$bins_GantryAngle, '"')
                         names(updated_data)[names(updated_data) == "bins_GantryAngle"] <- "Gantry_Angle"
                         updated_data$bins_MU_deg<- paste0('"', updated_data$bins_MU_deg, '"')
                         names(updated_data)[names(updated_data) == "bins_MU_deg"] <- "MU_deg"
                         updated_data$bins_MU_cGy<- paste0('"', updated_data$bins_MU_cGy, '"')
                         names(updated_data)[names(updated_data) == "bins_MU_cGy"] <- "MU_cGy"
                         updated_data$bins_SSD<- paste0('"', updated_data$bins_SSD, '"')
                         names(updated_data)[names(updated_data) == "bins_SSD"] <- "SSD"
                         updated_data$bins_Dose_Per_Fraction<- paste0('"', updated_data$bins_Dose_Per_Fraction, '"')
                         names(updated_data)[names(updated_data) == "bins_Dose_Per_Fraction"] <- "Dose_Per_Fraction"
                         # updated_data$Dose_Per_Fraction<-NULL
                         names(updated_data)[names(updated_data) == "T_stage"] <- "T_Stage"
                         names(updated_data)[names(updated_data) == "N_stage"] <- "N_Stage"
                         names(updated_data)[names(updated_data) == "M_stage"] <- "M_Stage"
                         
                         
                         names(updated_data)[names(updated_data) == "Anatomic_tumour_loc"] <- "Anatomic_Tumor_Location"
                         names(updated_data)[names(updated_data) == "PTV_Dose_Rx"] <- "PTV_Dose"
                         names(updated_data)[names(updated_data) == "Total_Fractions"] <- "Number_of_Fractions"
                         names(updated_data)[names(updated_data) == "Tolerance"] <- "Tolerance_Table"
                         
                         names(updated_data)[names(updated_data) == "Number_of_beams"] <- "Number_of_Beams"
                         names(updated_data)[names(updated_data) == "Wedge"] <- "Wedges"
                         names(updated_data)[names(updated_data) == "Rx_Radiation_Type"] <- "Radiation_Type"
                         names(updated_data)[names(updated_data) == "MU_deg"] <- "MU_Per_Degree"
                         
                         
                         
                         
                         
                         
                         names(updated_data)[names(updated_data) == "MU_cGy"] <- "MU_Per_cGy"
                         updated_data$PTV_Dose<-NULL
                         names(updated_data)[names(updated_data) == "bins_PTV_Dose_Rx"] <- "PTV_Dose"
                         
                         
                         updated_data$PTV_Dose<- paste0('"', updated_data$PTV_Dose, '"')
                         
                         
                         
                         names(updated_data)[names(updated_data) == "PTV_Dose"] <- "PTV_Dose_Rx"
                         names(updated_data)[names(updated_data) == "Wedges"] <- "Wedge"
                         updated_data$Dose_Per_Fraction<-NULL
                         
                           
                         library(data.table)
                         ERRORS_DESCRIPTION <- updated_data[updated_data$errors_description %like% "Should have been", ]        # Extract matching rows with %like%
                         
                         
                         DATAFRAME1<-separate_rows(ERRORS_DESCRIPTION, errors_description, sep = ",")
                         DATAFRAME1 <- DATAFRAME1[DATAFRAME1$errors_description %like% "Should have been ", ]   
                         
                         
                         
                         
                         #keep only the columns I need
                         DATAFRAME1 <- select(DATAFRAME1, Researchnumber,
                                              errors,
                                              errors_description)
                         updated_data$errors_description<-NULL
                         # Extract matching rows with %like%
                         newtable <- merge(DATAFRAME1,updated_data, by=c("Researchnumber","errors"), all=T) 
                         
                            
                         table(newtable$errors)
                         table(newtable$errors_description)
                         
                         # newtable$errors_description<-as.factor(as.character(newtable$errors_description))
                         #                          
                         # newtable$errors2<-ifelse(newtable$errors2[newtable$errors_description == NA], 0,1)
                         
                        
                         
                         
                         
                         
                         
                        
                         
                         
                         ####################################
                         ###########final check################
                         #####################################
                         
                         
                         table(newtable$Collimator_Angle)
                         table(newtable$Table_Angle)
                         table(newtable$MU_Per_Degree)
                         names(newtable)[names(newtable) == "MU_Per_Degree"] <- "MU_Per_Deg"
                         table(newtable$MU_Per_Deg)
                         names(newtable)[names(newtable) == "MU_Per_Deg"] <- "MU_Deg"
                         
                         table(newtable$Dose_Per_Fraction)
                         table(newtable$MU_Per_cGy)
                         names(newtable)[names(newtable) == "MU_Per_cGy"] <- "MU_cGy"
                         table(newtable$Number_of_Rxs)
                         require(plyr)
                         require(dplyr)
                         revalue(newtable$Number_of_Rxs, c("5"= "5 or more")) -> newtable$Number_of_Rxs
                         table(newtable$Control_Points)
                         table(newtable$Number_of_Beams)
                         table(newtable$Beam_Energy)
                         table(newtable$Tolerance_Table)
                         table(newtable$Wedge)
                         revalue(newtable$Wedge, c('"No"' = "no")) -> newtable$Wedge
                         revalue(newtable$Wedge, c("no" = '"no"')) -> newtable$Wedge
                         table(newtable$Orientation)
                         revalue(newtable$Orientation, c('"Head First-Supine"' = "HeadIn-Supine")) -> newtable$Orientation
                         table(newtable$Bolus)
                         revalue(newtable$Bolus, c('"No"' = "no")) -> newtable$Bolus
                         revalue(newtable$Bolus, c('"Yes"' = "yes")) -> newtable$Bolus
                         revalue(newtable$Bolus, c("no" = '"no"')) -> newtable$Bolus
                         revalue(newtable$Bolus, c("yes" = '"yes"')) -> newtable$Bolus
                         table(newtable$N_Stage)
                         revalue(newtable$N_Stage, c('"NX"' = '"Nx"')) -> newtable$N_Stage
                         table(newtable$Number_of_Fractions)
                         table(newtable$SSD)
                         table(newtable$Anatomic_Tumor_Location)
                         revalue(newtable$Anatomic_Tumor_Location, c('"Musculoskeletal (MSK)"' = '"Musculoskeletal"')) -> newtable$Anatomic_Tumor_Location
                         revalue(newtable$Anatomic_Tumor_Location, c('"Unknown primary"' = '"Others"')) -> newtable$Anatomic_Tumor_Location
                         table(newtable$PTV_Dose_Rx)
                         table(newtable$T_Stage)
                         revalue(newtable$T_Stage, c('"TX"' = '"Tx"')) -> newtable$T_Stage
                         table(newtable$Plan_Technique)
                         table(newtable$M_Stage)
                         revalue(newtable$M_Stage, c('"MX"' = '"Mx"')) -> newtable$M_Stage
                         table(newtable$Radiation_Type)
                         table(newtable$Treatment_Intent)
                         table(newtable$Gantry_Angle)

                         
                        