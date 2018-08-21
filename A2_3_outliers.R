#########################################################################################
# A2_HoaglinIglewicz_check
#########################################################################################
# This script reads in data from two sessions (1-2) from 6 tasks (A-F) and identifies
# outlier LI values using the Hoaglin Iglewicz method (Hoaglin & Iglewicz, 1987)

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
# Read in LI data

LI_data <- read.csv(paste0(dir,"LI_data",datatype,".csv"))

nsub <- dim(LI_data)[1]

exclusions  <- data.frame(
  "ID"      = as.factor(LI_data$ID),
  "Session" = as.factor(LI_data$Session),
  "A"       = rep(0, nsub),
  "B"       = rep(0, nsub),
  "C"       = rep(0, nsub),
  "D"       = rep(0, nsub),
  "E"       = rep(0, nsub),
  "F"       = rep(0, nsub))

# Mark tasks with < 12 useable trials as excluded
# NB: Only do this for averaged trial data! (Datatypes 1 and 2)
if (datatype < 3){
for (p in 1:nsub){
  for (t in 1:6){
    if (LI_data[p, t+8] < 12){
      exclusions[p, t+2] <- 1
    }
  }
}
}


# Change excluded trial data to NA before identifying Hoaglin Iglewicz outliers:

LI_data_clean <- LI_data[ , 1:8]

for (p in 1:nsub){
  for (t in 1:6){
    if (exclusions[p, t+2] == 1){
      LI_data_clean[p, t+2] = NA
    }
  }
}


########################################################
# Outliers are defined based on the standard error across trials.
# LI values more than 2.2 times the difference between the first and third quartiles (2.2 * (Q3-Q1)) 
# above the third quartile values are classed as outliers
# (e.g: upper limit = Q3 + 2.2*(Q3-Q1)). 

# The limits are based on data from ALL tasks
allse<-vector()

for (t in 1:6){
  allse<-c(allse,LI_data[,(t+14)])
}

lower_quartile <- quantile(allse, probs=0.25, na.rm="TRUE")
upper_quartile <- quantile(allse, probs=0.75, na.rm="TRUE")
quartile_diff <- upper_quartile - lower_quartile

upper_limit <- upper_quartile + 2.2*quartile_diff

for (t in 1:6){
  for (p in 1:(nsub)){
    if (is.na(LI_data_clean[p, t+2]) == 0 & LI_data[p, t+14] > upper_limit) { # For LI values higher than upper limit
      exclusions[p, t+2] <- 2
      }
  }
}

write.csv(exclusions, paste0(dir,"LI_exclusions", datatype, ".csv"), row.names=FALSE)

