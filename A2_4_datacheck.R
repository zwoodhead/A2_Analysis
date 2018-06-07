#########################################################################################
# A2 Script to analyse normality and reliability of LI_peak and LI_mean median values
#########################################################################################


########################################################
# Install packages

library(xlsx) #install.packages(c("xlsx","dplyr"))
require(dplyr)

dir<-("C:/Users/zwoodhead/Dropbox/Project A2/A2_Data/")
########################################################
# Loop through four different datatypes 
#
# 1 = Peak L-R diff, using grand mean
# 2 = Mean L-R diff, using grand mean
# 3 = Peak L-R diff, median of all trials
# 4 = Mean L-R diff, median of all trials

LI_stats <- matrix(data=NA, 8, 12)
rownames(LI_stats) <- c('data1_w', 'data1_p', 'data2_w', 'data2_p', 'data3_w', 'data3_p', 'data4_w', 'data4_p')

for (d in 1:4){


########################################################
# Read in LI data
LI_data <- read.csv(paste0(dir,"LI_data",d,".csv"))
LI_exclusions <- read.csv(paste0(dir,"LI_exclusions",d,".csv"))
nsub <- dim(LI_data)[1] / 2

# Reshape into 12 columns
LI_data <- cbind(LI_data[1:nsub, 3:8], LI_data[(nsub+1):(nsub*2), 3:8])
colnames(LI_data) <- c("A1","B1","C1","D1","E1","F1","A2","B2","C2","D2","E2","F2")

####################################################
# Normality and Reliability

# Test whether LI values are normally distributed

colnames(LI_stats) <- colnames(LI_data)

for (t in 1:12){
  
  # Shapiro Wilks W statistics
  LI_stats[d*2-1,t] <- round(shapiro.test(LI_data[1:nsub, t])$statistic, 3)
  
  # Shapiro Wilks p values
  LI_stats[d*2,t] <- round(shapiro.test(LI_data[1:nsub, t])$p.value, 5)
  
}

}