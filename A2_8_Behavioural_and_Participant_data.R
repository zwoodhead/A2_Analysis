#########################################################################################
# A2 Script to analyse behavioural data and participant info from tasks
#########################################################################################


########################################################
# Install packages

library(xlsx) #install.packages(c("xlsx","dplyr"))
require(dplyr)
library(reshape2)

dir<-("C:/Users/zwoodhead/Dropbox/Project A2/A2_Data/")

########################################################
# Read in participant and behavioural data

participant_info <- read.csv(paste0(dir,"A2_Participant_Info.csv"), sep=",")
behavioural_data <- read.csv(paste0(dir,"A2_Behavioural_Data.csv"), sep=",")

rh_subjects <- which(participant_info$handedness=="R")
nsub <- length(rh_subjects)

########################################################
# Select only rh_subjects

participant_info <- participant_info[rh_subjects, ]
behavioural_data <- behavioural_data[rh_subjects, ]

########################################################
# Calculate mean age and count number of women

age_mean <- mean(participant_info$Age_m) / 12
age_sd <- sd(participant_info$Age_m) / 12

table(participant_info$Gender)

########################################################
# Calculate stats for tasks A and D

A_D_cols <- c(
  which(colnames(behavioural_data) == "A1.Words"),
  which(colnames(behavioural_data) == "A2.Words"),
  which(colnames(behavioural_data) == "D1.Words"),
  which(colnames(behavioural_data) == "D2.Words")
)

A_D_data <- behavioural_data[ , c(1,A_D_cols)]
A_D_data$Filename <- as.factor(A_D_data$Filename)

A_D_means <- colMeans(A_D_data[2:5], na.rm=TRUE)
A_D_sd <- apply(A_D_data[2:5], 2, sd, na.rm=TRUE)

A_D_long <- melt(A_D_data)
A_D_long$Task <- as.factor(c(rep("A",nsub*2), rep("D",nsub*2)))
A_D_long$Session <- as.factor(c(rep(1,nsub),rep(2,nsub),rep(1,nsub),rep(2,nsub)))


A_D_ANOVA <- aov(value ~ Session * Task 
                  + Error(Filename / (Session * Task)),
                 data=A_D_long)

summary(A_D_ANOVA)

########################################################
# Create table of accuracy and rt data for tasks B, C, E 
# and F (Table 1)

Table1 <- data.frame(
  'Task'     = c("B","B","C","C","E","E","F","F"),
  'Session'  = c(1,2,1,2,1,2,1,2),
  'Accuracy' = rep(1,8),
  'Acc_sd'   = rep(1,8),
  'RT'       = rep(1,8),
  'RT_sd'   = rep(1,8)
)

Table1$Task    <- as.factor(Table1$Task)
Table1$Session <- as.factor(Table1$Session)

Acc_col_nums <- c(
  which(colnames(behavioural_data) == 'B1.Acc'),
  which(colnames(behavioural_data) == 'B2.Acc'),
  which(colnames(behavioural_data) == 'C1.Acc'),
  which(colnames(behavioural_data) == 'C2.Acc'),
  which(colnames(behavioural_data) == 'E1.Acc'),
  which(colnames(behavioural_data) == 'E2.Acc'),
  which(colnames(behavioural_data) == 'F1.Acc'),
  which(colnames(behavioural_data) == 'F2.Acc')
)
Table1$Accuracy <- colMeans(behavioural_data[Acc_col_nums], na.rm=TRUE)
Table1$Acc_sd   <- apply(behavioural_data[Acc_col_nums], 2, sd, na.rm=TRUE)

RT_col_nums <- c(
  which(colnames(behavioural_data) == 'B1.RT'),
  which(colnames(behavioural_data) == 'B2.RT'),
  which(colnames(behavioural_data) == 'C1.RT'),
  which(colnames(behavioural_data) == 'C2.RT'),
  which(colnames(behavioural_data) == 'E1.RT'),
  which(colnames(behavioural_data) == 'E2.RT'),
  which(colnames(behavioural_data) == 'F1.RT'),
  which(colnames(behavioural_data) == 'F2.RT')
)
Table1$RT <- colMeans(behavioural_data[RT_col_nums], na.rm=TRUE)
Table1$RT_sd   <- apply(behavioural_data[RT_col_nums], 2, sd, na.rm=TRUE)

########################################################
# Test for differences between sessions



