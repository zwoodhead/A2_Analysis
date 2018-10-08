#Max likelihood for laterality models
#by DVM Bishop, started 6th March 2018; updated 8th March 2018

#Prereg document at: https://osf.io/9uaw4/register/565fb3678c5e4a66b5582f67 

#Needs OpenMx, which you get with following command (not CRAN)
#source('http://openmx.psyc.virginia.edu/getOpenMx.R')
require(tidyverse)
require(OpenMx)
require(stargazer) #simple commands for nice tables
require(semTools)
library(DiagrammeR) #for the diagram
library('xlsx')
require(stringr)
library(semPlot)

########################################################
# Select LI data type: 
#
# 1 = Peak L-R diff, using grand mean
# 2 = Mean L-R diff, using grand mean
# 3 = Peak L-R diff, median of all trials
# 4 = Mean L-R diff, median of all trials

datatype <- 2 #as.numeric(readline("Which data type? 1=peak, 2=mean, 3=median peak, 4=median mean:   "))
dir<-("C:/Users/zwoodhead/Dropbox/Project A2/A2_Data/")
outdir <- paste0(dir,'SEM',datatype)

nsub <- 30 


##########################################################################
# Read in data
# NB. Need to set working directory to location of data files - or else specify path

alltask <- read.csv(paste0(dir,'LI_data', datatype, '.csv'))
alltask <- cbind(alltask[1:nsub, 3:8], alltask[(nsub+1):(nsub*2), 3:8]) # Reshape into 12 columns

mylabels<-c('ListGen1','PhonDec1','SemDec1','SentGen1','SentComp1','Jabber1',
            'ListGen2','PhonDec2','SemDec2','SentGen2','SentComp2','Jabber2')

colnames(alltask)<-mylabels

# Exclude data with < 12 trials and outliers identified by H&I method
exclusions  <- read.csv(paste0(dir,"LI_exclusions", datatype, ".csv"))
exclusions  <- cbind(exclusions[1:nsub, 3:8], exclusions[(nsub+1):(nsub*2), 3:8]) # Reshape into 12 columns

for (t in 1:12){
  alltask[which(exclusions[ , t] > 0), t] = NA # Change excluded values to NA
}

# Set up summary table 
bigsummary1 <- data.frame(cbind(0,0,0,'Model Name'),row.names='SingleFactor',stringsAsFactors=FALSE)
colnames(bigsummary1)<-c('CFI','RMSEA','BIC','Comment')

bigsummary2 <- data.frame(cbind(0,0,0,'Model Name'),row.names='SingleFactor',stringsAsFactors=FALSE)
colnames(bigsummary2)<-c('CFI','RMSEA','BIC','Comment')

# Set up table of results from Factor Models
singlefactor <- data.frame(
  name =   c("l2","l3","l4","l5","l6","e1","e2","e3","e4","e5","e6","varFactor1","meanA","meanB","meanC","meanD","meanE","meanF"),
  matrix = c("A","A","A","A","A","S","S","S","S","S","S","S","M","M","M","M","M","M"),
  from = c("PD1","SD1","SG1","SC1","JD1",  "LD1","PD1","SD1","SG1","SC1","JD1",  "F1",1,1,1,1,1,1),
  to = c("F1","F1","F1","F1","F1","LD1","PD1","SD1","SG1","SC1","JD1","F1", "LD1","PD1","SD1","SG1","SC1","JD1")
)

twofactor <- data.frame(
  name = c("k2","k3","k4","k5","k6","l2","l3","l4","l5","l6", "e1","e2","e3","e4","e5","e6","varFactor1","meanA","meanB","meanC","meanD","meanE","meanF"),
  matrix = c("A","A","A","A","A","A","A","A","A","A","S","S","S","S","S","S","S","M","M","M","M","M","M"),
  from = c("PD1","SD1","SG1","SC1","JD1","PD1","SD1","SG1","SC1","JD1",  "LD1","PD1","SD1","SG1","SC1","JD1","F1",1,1,1,1,1,1),
  to = c("F1","F1","F1","F1","F1","F2","F2","F2","F2","F2","LD1","PD1","SD1","SG1","SC1","JD1", "F1", "LD1","PD1","SD1","SG1","SC1","JD1")
)

# Create new text file to cat to
cat(paste0("SEM",datatype," Loop Summary\n\n"), file = "loop.txt", append = FALSE)

# Read in template for grViz
mybit<-read.csv(paste0(dir,'for_graphviz.csv'),stringsAsFactors = FALSE,header=FALSE) #full list of all paths.

#------------------------------------------------------------------
#This bit of script was used at the start of OpenMx in case we wanted to try 'drop one' approach
#to test consistency of bifactor solution. It will drop cases specified in thisdrop.
#Could do this in a loop once we have an optimal approach.
for (thisdrop in 1:30){   #specify a number for participant to be dropped
  
  cat(paste0("\nDropping subject ", thisdrop, "\n"), file = "SEM4loop.txt", append = TRUE)

  dataRaw      <- mxData( observed=alltask[-thisdrop,], type="raw" )

  #show means etc for tasks with time1 and time2 adjacent
  alltask_dropone <- alltask[-thisdrop, c(1,7,2,8,3,9,4,10,5,11,6,12)]
  cat(stargazer(alltask_dropone,type='text'), file = "SEM4loop.txt", fill = 40, append = TRUE)

#---------------------------------------------------------------------------------------------------------------------------
# The following models consider covariances
# Single factor model (means equalized for t1 and t2)
# -----------------------------------------------------------------------

# residual variances
  resVars      <- mxPath( from=mylabels, arrows=2,
                          free=c(T,T,T,T,T,T,T,T,T,T,T,T), values=c(1,1,1,1,1,1,1,1,1,1,1,1),
                          labels=c("e1","e2","e3","e4","e5","e6","e1","e2","e3","e4","e5","e6") ) # variances are fixed for task/session

# latent variance - Factor1 is the single factor
latVar       <- mxPath( from="Factor1", arrows=2,
                        free=TRUE, values=1, labels ="varFactor1" )

# factor loadings
facLoads     <- mxPath( from="Factor1", to=mylabels, arrows=1,
                        free=c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE), 
                        values=c(1,1,1,1,1,1,1,1,1,1,1,1),
                        labels =c("l1","l2","l3","l4","l5","l6","l1","l2","l3","l4","l5","l6") )#same for each test on time 1 and 2
#The first path is fixed at one - others scaled relative to this

# means - one extra mean for the Factor, but this is set to NA
means        <- mxPath( from="one", to=c(mylabels,'Factor1'), arrows=1,
                        free=c(T,T,T,T,T,T,T,T,T,T,T,T,FALSE), values=c(1,1,1,1,1,1,1,1,1,1,1,1,0),
                        labels =c("meanA","meanB","meanC",
                                  "meanD","meanE","meanF","meanA","meanB","meanC",
                                  "meanD","meanE","meanF",NA) ) #means constant from time 1 to time 2

oneFactorModel <- mxModel("Single Factor Model", type="RAM",
                          manifestVars=mylabels, latentVars="Factor1",
                          dataRaw, resVars, latVar, facLoads, means)

oneFactorFit<-mxRun(oneFactorModel)
summaryF1<-summary(oneFactorFit)

singlefactor <- cbind(singlefactor, summaryF1$parameters$Estimate, summaryF1$parameters$Std.Error)

myCFI <-round(fitMeasuresMx(oneFactorFit)[7],3)
myrmsea <-round(fitMeasuresMx(oneFactorFit)[21],3)
BICF1 <- round(summaryF1$BIC.Mx,1)

bigsummary1[thisdrop,] <- c(myCFI,myrmsea,BICF1,paste0("Single Factor, dropping ", thisdrop))


#---------------------------------------------------------------------------------------------------------------------------
#Bifactor model 
# -----------------------------------------------------------------------

# residual variances
resVars      <- mxPath( from=mylabels, arrows=2,
                        free=c(T,T,T,T,T,T,T,T,T,T,T,T), values=c(1,1,1,1,1,1,1,1,1,1,1,1),
                        labels=c("e1","e2","e3","e4","e5","e6","e1","e2","e3","e4","e5","e6") ) # variances are fixed at each task/session

# latent variances and covariance: NB assume totally independent, so covariance fixed at zero
latVars      <- mxPath( from=c("Factor1","Factor2"), arrows=2, connect="unique.pairs",
                        free=c(T,F,F), values=c(1,0,1), labels=c("varFactor1","cov","varFactor2") )

# factor loadings for Factor1 #NB test A loading is fixed to one for this factor
facLoadsFactor1     <- mxPath( from="Factor1", to=mylabels, arrows=1,
                               free=c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE), 
                               values=rep(1,12),
                               labels =c("k1","k2","k3","k4","k5","k6","k1","k2","k3","k4","k5","k6") )

# factor loadings for Factor2 #NB test A loading is fixed to zero for this factor
facLoadsFactor2     <- mxPath( from="Factor2", to=mylabels, arrows=1,
                               free=c(FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE), 
                               values=c(0,rep(1,5),0,rep(1,5)),
                               labels =c("l1","l2","l3","l4","l5","l6","l1","l2","l3","l4","l5","l6") )

# means #estimated for all except the two factors
means        <- mxPath( from="one", to=c(mylabels,'Factor1','Factor2'), arrows=1,
                        free=c(T,T,T,T,T,T,T,T,T,T,T,T,FALSE,FALSE), values=c(1,1,1,1,1,1,1,1,1,1,1,1,0,0),
                        labels =c("meanA","meanB","meanC",
                                  "meanD","meanE","meanF","meanA","meanB","meanC",
                                  "meanD","meanE","meanF",NA,NA) )

biFactorModel <- mxModel("BiFactor Model", type="RAM",
                         manifestVars=mylabels,
                         latentVars=c("Factor1","Factor2"),
                         dataRaw, resVars, latVars, facLoadsFactor1, facLoadsFactor2, means)

biFactorFit <- mxRun(biFactorModel)
summarybiF<-summary(biFactorFit)
mysummary<-summarybiF$parameters[1:10,c(1,3:6)]
mysummary$z<-mysummary$Estimate/mysummary$Std.Error


twofactor <- cbind(twofactor, summarybiF$parameters$Estimate, summarybiF$parameters$Std.Error)

myCFI <-round(fitMeasuresMx(biFactorFit)[7],3)
myrmsea <-round(fitMeasuresMx(biFactorFit)[21],3)
BICF2 <- round(summarybiF$BIC.Mx,1)

bigsummary2[thisdrop,] <- c(myCFI,myrmsea,BICF2,paste0("Two Factors, dropping ", thisdrop))

mcomp<-mxCompare(biFactorFit,oneFactorFit)
pmessage<-'Bi-factor model does not improve fit over one factor model'

if (mcomp$p[2]<.05){pmessage <- paste0('Bi-factor model (BIC=',BICF2,') is better fit than one factor model (BIC=',BICF1,')')}
cat(paste0("\n Dropping subject ", thisdrop, ": ", pmessage, "\n\n"), file = "SEM4loop.txt", append = TRUE)
write.table(mcomp, file = "SEM4loop.txt", append= TRUE, quote = FALSE)


#--------------------------------------------------------------------------------------------
# draw diagram of the bi-factor model, with NS paths omitted
# A shown in red as this has fixed paths to X1 (1) and X2 (0)
#--------------------------------------------------------------------------------------------

# omxGraphviz(biFactorModel, dotFilename = "bifactor.dot")
# grViz("bifactor.dot") #this will generate a .dot file but it
# is messy, as it shows time 1 and time 2 measures, as well as means

# Script below shows time1/time2 combined and omits means for clarity
# The file for_graphviz is set up in advance and read in and  modified according to results
mybit2<-print.data.frame(mybit, 
                         quote=FALSE) #get rid of quotes

#we now want to a) remove rows that are NS and b) put in path coeffs for the rest
thisrow<-12 #NB: first row with path specification is col 13
thatrow<-0 #counter for the summary z scores: NB these exclude measure A! 
for (j in 1:2){#each *factor* (not each test occasion - these are collapsed in diagram)
  
  for (i in 1:6){ #each task 
    thisrow<-thisrow+1
    
    if(i>1){ #measure A is fixed so not in the table
      thatrow<-thatrow+1
      if(mysummary$z[thatrow]<1.65)
      {mybit2[thisrow,]<-''} #delete this one
      else{
        pathlabel<-round(mysummary$Estimate[thatrow],2)
        bb<-mybit2[thisrow,]
        bb<-str_replace(bb,'xx',as.character(pathlabel))
        mybit2[thisrow,]<-bb
      }
    } #loop to here when i is 1: no action
  }
}
mybit2[19,]<-'' #delete path for A to X2: this one was fixed to 0
dotFilename<-paste0(outdir,'forgrViz_SEM',datatype,'_',thisdrop,'.dot')
write.table(mybit2, dotFilename, append = FALSE,
            row.names = FALSE, col.names = FALSE,quote=FALSE)


} # End loop of iterations


#--------------------------------------------------------------------------------------------
#Save summary tables
#--------------------------------------------------------------------------------------------

colnames(singlefactor) <- c('name','matrix','from','to',
                            'Mean_drop1','SE_drop1','Mean_drop2','SE_drop2','Mean_drop3','SE_drop3','Mean_drop4','SE_drop4','Mean_drop5','SE_drop5',
                            'Mean_drop6','SE_drop6','Mean_drop7','SE_drop7','Mean_drop8','SE_drop8','Mean_drop9','SE_drop9','Mean_drop10','SE_drop10',
                            'Mean_drop11','SE_drop11','Mean_drop12','SE_drop12','Mean_drop13','SE_drop13','Mean_drop14','SE_drop14','Mean_drop15','SE_drop15',
                            'Mean_drop16','SE_drop16','Mean_drop17','SE_drop17','Mean_drop18','SE_drop18','Mean_drop19','SE_drop19','Mean_drop20','SE_drop20',
                            'Mean_drop21','SE_drop21','Mean_drop22','SE_drop22','Mean_drop23','SE_drop23','Mean_drop24','SE_drop24','Mean_drop25','SE_drop25',
                            'Mean_drop26','SE_drop26','Mean_drop27','SE_drop27','Mean_drop28','SE_drop28','Mean_drop29','SE_drop29','Mean_drop30','SE_drop30')
write.csv(singlefactor, file = paste0(outdir,"loop_parameters1.csv"))

colnames(twofactor) <- c('name','matrix','from','to',
                            'Mean_drop1','SE_drop1','Mean_drop2','SE_drop2','Mean_drop3','SE_drop3','Mean_drop4','SE_drop4','Mean_drop5','SE_drop5',
                            'Mean_drop6','SE_drop6','Mean_drop7','SE_drop7','Mean_drop8','SE_drop8','Mean_drop9','SE_drop9','Mean_drop10','SE_drop10',
                            'Mean_drop11','SE_drop11','Mean_drop12','SE_drop12','Mean_drop13','SE_drop13','Mean_drop14','SE_drop14','Mean_drop15','SE_drop15',
                            'Mean_drop16','SE_drop16','Mean_drop17','SE_drop17','Mean_drop18','SE_drop18','Mean_drop19','SE_drop19','Mean_drop20','SE_drop20',
                            'Mean_drop21','SE_drop21','Mean_drop22','SE_drop22','Mean_drop23','SE_drop23','Mean_drop24','SE_drop24','Mean_drop25','SE_drop25',
                            'Mean_drop26','SE_drop26','Mean_drop27','SE_drop27','Mean_drop28','SE_drop28','Mean_drop29','SE_drop29','Mean_drop30','SE_drop30')
write.csv(twofactor, file = paste0(outdir,"loop_parameters2.csv"))

write.csv(bigsummary1, file = paste0(outdir,"loop_bigsummary1.csv"))
write.csv(bigsummary2, file = paste0(outdir,"loop_bigsummary2.csv"))


#--------------------------------------------------------------------------------------------
#Printing Figures (run manually)
#--------------------------------------------------------------------------------------------


# datatype <- 2 # Change as required
# thisdrop <- 1 # Manually run from 1 to nsub
## May need to change into SEM directory to find file easily
# dotFilename<-paste0('forgrViz_SEM',datatype,'_',thisdrop,'.dot')
# grViz(dotFilename)




