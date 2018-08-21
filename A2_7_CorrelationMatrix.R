####################################################
# Script to create Correlation matrix of A2 LI values
#
####################################################

##########################################################################
# Load packages

library("reshape2")
library("ggplot2")


##########################################################################
# Read in data
# NB. Need to set working directory to location of data files - or else specify path

datatype <- as.numeric(readline("Which data type? 1=peak, 2=mean, 3=median peak, 4=median mean:   "))
dir<-("C:/Users/zwoodhead/Dropbox/Project A2/A2_Data/")

nsub <- 30 #change this when all data are collected

alltask <- read.csv(paste0(dir,'LI_data', datatype, '.csv'))

alltask <- cbind(alltask[1:nsub, 3], 
                 alltask[(nsub+1):(nsub*2), 3],
                 alltask[1:nsub, 4], 
                 alltask[(nsub+1):(nsub*2), 4],
                 alltask[1:nsub, 5], 
                 alltask[(nsub+1):(nsub*2), 5],
                 alltask[1:nsub, 6], 
                 alltask[(nsub+1):(nsub*2), 6],
                 alltask[1:nsub, 7], 
                 alltask[(nsub+1):(nsub*2), 7],
                 alltask[1:nsub, 8], 
                 alltask[(nsub+1):(nsub*2), 8]) # Reshape into 12 columns: order A1, A2, B1, B2, C1, C2 etc.

mylabels<-c('A1','A2','B1','B2','C1','C2',
            'D1','D2','E1','E2','F1','F2')

colnames(alltask)<-mylabels


##########################################################################
# Change excluded data to NA

exclusions  <- read.csv(paste0(dir,"LI_exclusions", datatype, ".csv"))
exclusions  <- cbind(exclusions[1:nsub, 3], 
                     exclusions[(nsub+1):(nsub*2), 3],
                     exclusions[1:nsub, 4], 
                     exclusions[(nsub+1):(nsub*2), 4],
                     exclusions[1:nsub, 5], 
                     exclusions[(nsub+1):(nsub*2), 5],
                     exclusions[1:nsub, 6], 
                     exclusions[(nsub+1):(nsub*2), 6],
                     exclusions[1:nsub, 7], 
                     exclusions[(nsub+1):(nsub*2), 7],
                     exclusions[1:nsub, 8], 
                     exclusions[(nsub+1):(nsub*2), 8]) # Reshape into 12 columns

colnames(exclusions)<-mylabels

for (t in 1:12){
  alltask[which(exclusions[ , t] > 0), t] = NA # Change excluded values to NA
}

##########################################################################
# Create correlation matrix

cormat <- round(cor(alltask, use = "pairwise.complete.obs", method = "spearman"), 2)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri,na.rm=T)

##########################################################################
# Create correlation heatmap using ggplot

png(filename=paste0(dir,"Figures/CorrMatrix_",datatype,".png"))
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(colour = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(vjust = 1, 
                                   size = 14, hjust = 0.5),
        axis.text.y = element_text(vjust = 0.5, 
                                   size = 14, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 9, barheight = 1.5,
                               title.position = "top", title.hjust = 0.5))

dev.off()

### Extra Plot

# plot(alltask[,1:2], type='n', ylim = c(-4,5), xlim=c(.5, 2.5), axes=FALSE, ann=FALSE)
# 
# for (s in 1:nsub){lines(alltask[s,1:2], type='b', pch=1, lty=1, lwd=.5)}
# axis(1, at=1:2, labels=c('Session1', 'Session2'), tick=FALSE)
# 
# axis(2, at=-4:5, lwd=2)
# abline(h=-4, lwd=2)
