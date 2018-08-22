#########################################################################################
# A2 Script to analyse behavioural data and participant info from tasks
#########################################################################################


########################################################
# Install packages

library(xlsx) #install.packages(c("xlsx","dplyr"))
require(dplyr)
library(reshape2)
library(stringr)

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

A_D_cols <- grep('*.Words', colnames(behavioural_data)) 

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
# Count number of omitted trials (trials with no response) 
# for each task

Omission_cols <- grep('*.Omit', colnames(behavioural_data))

Omission_data <- behavioural_data[ , Omission_cols]

Omission_sum <- colSums(Omission_data, na.rm=TRUE)
nevents <- c(15,15,15,15,90,90,90,45,90,90,90,45) # The number of events in each condition
Omission_perc <- colSums(Omission_data, na.rm=TRUE) / (nevents*nsub) * 100


########################################################
# Create table of accuracy and rt data for tasks B, C, E 
# and F (Table 1)

Table1 <- data.frame(
  'Task'     = c("B","C","E","F","B","C","E","F"),
  'Session'  = c(1,1,1,1,2,2,2,2),
  'Accuracy' = rep(1,8),
  'Acc_sd'   = rep(1,8),
  'RT'       = rep(1,8),
  'RT_sd'   = rep(1,8),
  'Omit_perc' = Omission_perc[5:12]
)

Table1$Task    <- as.factor(Table1$Task)
Table1$Session <- as.factor(Table1$Session)

Acc_col_nums <- grep('*.Acc', colnames(behavioural_data))

Table1$Accuracy <- colMeans(behavioural_data[Acc_col_nums], na.rm=TRUE)
Table1$Acc_sd   <- apply(behavioural_data[Acc_col_nums], 2, sd, na.rm=TRUE)

RT_col_nums <- grep('*.RT', colnames(behavioural_data))

Table1$RT <- colMeans(behavioural_data[RT_col_nums], na.rm=TRUE)
Table1$RT_sd   <- apply(behavioural_data[RT_col_nums], 2, sd, na.rm=TRUE)

# Re-order Table
positions <- order(Table1$Task)
Table1 <- Table1[positions, ]

Table1



