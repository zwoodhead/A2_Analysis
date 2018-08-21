####################################################
# Script to create Pirate Plot of LI values from
# A2 Results files
#
####################################################

# install.packages("devtools")
# install.packages("reshape2")
# install_github('ndphillips/yarrr')
library('devtools')
library("reshape2")
library("yarrr")

####################################################
# Prepare data
nsub <- 30

# Read data from LI_peak_data.csv file and exclusions from LI_peak_exclusions.csv

dir<-("C:/Users/zwoodhead/Dropbox/Project A2/A2_Data/")
datatype <- as.numeric(readline("Which data type? 1=peak, 2=mean, 3=median peak, 4=median mean:   "))


LI_data_all <- read.csv(paste0(dir,"LI_data",datatype,".csv"))
LI_data_all <- LI_data_all[,1:8]
LI_data_all$ID <- as.factor(LI_data_all$ID)
LI_data_all$Session <- as.factor(LI_data_all$Session)

exclusions  <- read.csv(paste0(dir,"LI_exclusions",datatype,".csv"))


# Create clean data with exclusions removed for stats
LI_data_clean <- LI_data_all
noutliers <- length(which(exclusions[ , 3:8] !=0)) # Counts how many exclusions aren't zero
xmarker_nums <- c(1,2,4,5,7,8,10,11,13,14,16,17) # For plotting x-axis
outlier_markers <- rep(0, noutliers)
outlier_values  <- rep(0, noutliers)
outliercount <-1

# Identify outliers for plotting
#Start with Session1
for (p in 1:(nsub*2)){
  for (t in 1:6){
    if (exclusions[p, t+2] > 0) {
      LI_data_clean[p, t+2] = NA
      outlier_markers[outliercount] <- xmarker_nums[t*2 - 1] # Xmarker, Session1
      if (p > nsub){
        outlier_markers[outliercount] <- xmarker_nums[t*2]} # Xmarker, Session2
      outlier_values[outliercount] <- LI_data_all[p,t+2] # LI value
      outliercount <- outliercount+1
    }
  }
}


####################################################
# Normality and Wilcoxon tests

# Test whether LI values are normally distributed, and which LI means are significantly different to zero
LI_stats <- matrix(data=NA, 7, 12)
LI_stats[6,] <- rep("*",12)
rownames(LI_stats) <- c('shapiro_w', 'shapiro_p', 'wilcox_V', 'wilcox_p', 'label_height', 'label', 'n')
colnames(LI_stats) <- c("A1","A2","B1","B2","C1","C2","D1","D2","E1","E2","F1","F2")

for (t in 1:6){
  # Select relevant data
  Session1 <- LI_data_clean[1:nsub, t+2]
  Session2 <- LI_data_clean[(nsub+1):(nsub*2), t+2]
  
  # Shapiro Wilks W statistics
  LI_stats[1,2*t-1] <- round(shapiro.test(Session1)$statistic, 3)
  LI_stats[1,2*t]   <- round(shapiro.test(Session2)$statistic, 3)
  
  # Shapiro Wilks p values
  LI_stats[2,2*t-1] <- round(shapiro.test(Session1)$p.value, 5)
  LI_stats[2,2*t]   <- round(shapiro.test(Session2)$p.value, 5)
    
  # Wilcoxon test W statistics
  LI_stats[3,2*t-1] <- round(wilcox.test(Session1)$statistic, 3)
  LI_stats[3,2*t]   <- round(wilcox.test(Session2)$statistic, 3)
  
  # Wilcoxon test p values
  LI_stats[4,2*t-1] <- round(wilcox.test(Session1)$p.value, 5)
  LI_stats[4,2*t]   <- round(wilcox.test(Session2)$p.value, 5)
  
  # plot label height (data max + 1)
  LI_stats[5,t*2-1] <- max(Session1, na.rm=TRUE) + 1
  LI_stats[5,t*2]   <- max(Session2, na.rm=TRUE) + 1  
  
  # plot labels (* p<.05, ** p <.005)
  if      (as.numeric(LI_stats[4,t*2-1]) < 0.005) {
    LI_stats[6,t*2-1] <- "***"}
  
  else if (as.numeric(LI_stats[4,t*2-1]) > 0.05) {
    LI_stats[6,t*2-1] <- ""}
  
  if      (as.numeric(LI_stats[4,t*2]) < 0.001) {
    LI_stats[6,t*2] <- "***"}
  
  else if (as.numeric(LI_stats[4,t*2]) > 0.05) {
    LI_stats[6,t*2] <- ""}  
  
  # number of participants
  LI_stats[7, t*2-1] <- length(na.omit(Session1))
  LI_stats[7, t*2]   <- length(na.omit(Session2))
  
}


####################################################
# Pirate Plot

# Melt cleverly reshapes LI_data into a long format using ID 
# (the only factor) as the categorical variable    

LI_long <- melt(LI_data_clean)
colnames(LI_long) <- c("ID","Session","Task", "LI_value")

# Make pirate plot
png(filename=paste0(dir,"Figures/PiratePlot_",datatype,".png"))
pirateplot(formula = LI_value ~ Session + Task,
           data = LI_long,
           ylab = "LI Value",
           main = "Pirate Plot of LI Values",
           ylim = c(-3, 7))

# Mark significant differences (excluding outlier data) with black astrisks
text(xmarker_nums, y=as.numeric(LI_stats[5,1:12]), labels=LI_stats[6,1:12],font=2, cex=1.3)

# Mark outlier values as red dots
points(outlier_markers, y=outlier_values, type = "p", pch = 16, col="red")

dev.off()




