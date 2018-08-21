#########################################################################################
# A2 Script 2: Prepare LI data for further analysis
#########################################################################################

########################################################
# Install packages

library(xlsx) #install.packages(c("xlsx","dplyr"))
require(dplyr)

########################################################
# Select LI data type: 
#
# 1 = Peak L-R diff, using grand mean
# 2 = Mean L-R diff, using grand mean
# 3 = Peak L-R diff, median of all trials
# 4 = Mean L-R diff, median of all trials

datatype <- as.numeric(readline("Which data type? 1=peak, 2=mean, 3=median peak, 4=median mean:   "))
dir<-("C:/Users/zwoodhead/Dropbox/Project A2/A2_Data/")

########################################################
# Read in relevant results file

# Datatypes 1 and 2 use Results_SessionX.xlsx files
if (datatype < 3){
  mydata1 <- read.xlsx(paste0(dir,"Results_Session1.xlsx"), sheetIndex = 1)
  mydata2 <- read.xlsx(paste0(dir,"Results_Session2.xlsx"), sheetIndex = 1)
  
}

########################################################
# Select right handers only

participant_info <- read.csv(paste0(dir,"A2_Participant_Info.csv"), sep="")

rh_subjects <- which(participant_info$handedness=="R")

mydata1 <- mydata1[rh_subjects, ]
mydata2 <- mydata2[rh_subjects, ]

nsub <- dim(mydata2)[1]

########################################################
# Select LI data

# Datatype 1 == Peak LI values
if (datatype ==1){
  # Select relevant columns and put into new data frame (LI_data)
  LI_data1 <- data.frame("ID" = as.factor(mydata1$Filename),
                         "Session" = as.factor(rep(1, nsub)),
                         "A" = mydata1$A1.LI, "B" = mydata1$B1.LI, "C" = mydata1$C1.LI,
                         "D" = mydata1$D1.LI, "E" = mydata1$E1.LI, "F" = mydata1$F1.LI,
                         "An" = mydata1$A1.N, "Bn" = mydata1$B1.N, "Cn" = mydata1$C1.N,
                         "Dn" = mydata1$D1.N, "En" = mydata1$E1.N, "Fn" = mydata1$F1.N,
                         "Ase" = mydata1$A1.se, "Bse" = mydata1$B1.se, "Cse" = mydata1$C1.se,
                         "Dse" = mydata1$D1.se, "Ese" = mydata1$E1.se, "Fse" = mydata1$F1.se )
  
  LI_data2 <- data.frame("ID" = as.factor(mydata2$Filename),
                         "Session" = as.factor(rep(2, nsub)),
                         "A" = mydata2$A2.LI, "B" = mydata2$B2.LI, "C" = mydata2$C2.LI,
                         "D" = mydata2$D2.LI, "E" = mydata2$E2.LI, "F" = mydata2$F2.LI,
                         "An" = mydata2$A2.N, "Bn" = mydata2$B2.N, "Cn" = mydata2$C2.N,
                         "Dn" = mydata2$D2.N, "En" = mydata2$E2.N, "Fn" = mydata2$F2.N,
                         "Ase" = mydata2$A2.se, "Bse" = mydata2$B2.se, "Cse" = mydata2$C2.se,
                         "Dse" = mydata2$D2.se, "Ese" = mydata2$E2.se, "Fse" = mydata2$F2.se )
  }

# Datatype 2 == Mean LI values
if (datatype == 2){
  # Select relevant columns and put into new data frame (LI_data)
  LI_data1 <- data.frame("ID" = as.factor(mydata1$Filename),
                         "Session" = as.factor(rep(1, nsub)),
                         "A" = mydata1$A1.LI_mean, "B" = mydata1$B1.LI_mean, "C" = mydata1$C1.LI_mean,
                         "D" = mydata1$D1.LI_mean, "E" = mydata1$E1.LI_mean, "F" = mydata1$F1.LI_mean,
                         "An" = mydata1$A1.N, "Bn" = mydata1$B1.N, "Cn" = mydata1$C1.N,
                         "Dn" = mydata1$D1.N, "En" = mydata1$E1.N, "Fn" = mydata1$F1.N,
                         "Ase" = mydata1$A1.mean_se, "Bse" = mydata1$B1.mean_se, "Cse" = mydata1$C1.mean_se,
                         "Dse" = mydata1$D1.mean_se, "Ese" = mydata1$E1.mean_se, "Fse" = mydata1$F1.mean_se)
  
  LI_data2 <- data.frame("ID" = as.factor(mydata2$Filename),
                         "Session" = as.factor(rep(2, nsub)),
                         "A" = mydata2$A2.LI_mean, "B" = mydata2$B2.LI_mean, "C" = mydata2$C2.LI_mean,
                         "D" = mydata2$D2.LI_mean, "E" = mydata2$E2.LI_mean, "F" = mydata2$F2.LI_mean,
                         "An" = mydata2$A2.N, "Bn" = mydata2$B2.N, "Cn" = mydata2$C2.N,
                         "Dn" = mydata2$D2.N, "En" = mydata2$E2.N, "Fn" = mydata2$F2.N,
                         "Ase" = mydata2$A2.mean_se, "Bse" = mydata2$B2.mean_se, "Cse" = mydata2$C2.mean_se,
                         "Dse" = mydata2$D2.mean_se, "Ese" = mydata2$E2.mean_se, "Fse" = mydata2$F2.mean_se )
}

# Datatypes 3 and 4 use results from Results_MEDIAN_SessionX.xlsx
if (datatype > 2){
  mydata1 <- read.xlsx(paste0(dir,"Results_MEDIAN_Session1.xlsx"), sheetIndex = 1)
  mydata2 <- read.xlsx(paste0(dir,"Results_MEDIAN_Session2.xlsx"), sheetIndex = 1)
  
  nsub <- dim(mydata2)[1]
}

# Datatype 3 == median peak values
if (datatype == 3){
  # Select relevant columns and put into new data frame (LI_data)
  LI_data1 <- data.frame("ID" = as.factor(mydata1$Filename),
                         "Session" = as.factor(rep(1, nsub)),
                         "A" = mydata1$A1.LI_peak, "B" = mydata1$B1.LI_peak, "C" = mydata1$C1.LI_peak,
                         "D" = mydata1$D1.LI_peak, "E" = mydata1$E1.LI_peak, "F" = mydata1$F1.LI_peak,
                         "An" = mydata1$A1.N, "Bn" = mydata1$B1.N, "Cn" = mydata1$C1.N,
                         "Dn" = mydata1$D1.N, "En" = mydata1$E1.N, "Fn" = mydata1$F1.N,
                         "Ase" = mydata1$A1.mean_se, "Bse" = mydata1$B1.mean_se, "Cse" = mydata1$C1.mean_se,
                         "Dse" = mydata1$D1.mean_se, "Ese" = mydata1$E1.mean_se, "Fse" = mydata1$F1.mean_se)
  
  LI_data2 <- data.frame("ID" = as.factor(mydata2$Filename),
                         "Session" = as.factor(rep(2, nsub)),
                         "A" = mydata2$A2.LI_peak, "B" = mydata2$B2.LI_peak, "C" = mydata2$C2.LI_peak,
                         "D" = mydata2$D2.LI_peak, "E" = mydata2$E2.LI_peak, "F" = mydata2$F2.LI_peak,
                         "An" = mydata2$A2.N, "Bn" = mydata2$B2.N, "Cn" = mydata2$C2.N,
                         "Dn" = mydata2$D2.N, "En" = mydata2$E2.N, "Fn" = mydata2$F2.N,
                         "Ase" = mydata2$A2.mean_se, "Bse" = mydata2$B2.mean_se, "Cse" = mydata2$C2.mean_se,
                         "Dse" = mydata2$D2.mean_se, "Ese" = mydata2$E2.mean_se, "Fse" = mydata2$F2.mean_se )
}

# Datatype 4 == median mean values
if (datatype == 4){
  # Select relevant columns and put into new data frame (LI_data)
  LI_data1 <- data.frame("ID" = as.factor(mydata1$Filename),
                         "Session" = as.factor(rep(1, nsub)),
                         "A" = mydata1$A1.LI_mean, "B" = mydata1$B1.LI_mean, "C" = mydata1$C1.LI_mean,
                         "D" = mydata1$D1.LI_mean, "E" = mydata1$E1.LI_mean, "F" = mydata1$F1.LI_mean,
                         "An" = mydata1$A1.N, "Bn" = mydata1$B1.N, "Cn" = mydata1$C1.N,
                         "Dn" = mydata1$D1.N, "En" = mydata1$E1.N, "Fn" = mydata1$F1.N,
                         "Ase" = mydata1$A1.mean_se, "Bse" = mydata1$B1.mean_se, "Cse" = mydata1$C1.mean_se,
                         "Dse" = mydata1$D1.mean_se, "Ese" = mydata1$E1.mean_se, "Fse" = mydata1$F1.mean_se)
  
  LI_data2 <- data.frame("ID" = as.factor(mydata2$Filename),
                         "Session" = as.factor(rep(2, nsub)),
                         "A" = mydata2$A2.LI_mean, "B" = mydata2$B2.LI_mean, "C" = mydata2$C2.LI_mean,
                         "D" = mydata2$D2.LI_mean, "E" = mydata2$E2.LI_mean, "F" = mydata2$F2.LI_mean,
                         "An" = mydata2$A2.N, "Bn" = mydata2$B2.N, "Cn" = mydata2$C2.N,
                         "Dn" = mydata2$D2.N, "En" = mydata2$E2.N, "Fn" = mydata2$F2.N,
                         "Ase" = mydata2$A2.mean_se, "Bse" = mydata2$B2.mean_se, "Cse" = mydata2$C2.mean_se,
                         "Dse" = mydata2$D2.mean_se, "Ese" = mydata2$E2.mean_se, "Fse" = mydata2$F2.mean_se )
}

# Combine Session1 and Session2 data
LI_data_all <- rbind(LI_data1, LI_data2)

# Write to csv file
outname <- paste0(dir,"LI_data", datatype, ".csv")
write.csv(LI_data_all, outname, row.names=FALSE)