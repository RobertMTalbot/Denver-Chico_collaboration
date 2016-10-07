#imputed data analysis

library(dplyr)
library(nlme)

#JMN Read in
lvl1 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (laptop & home)
lvl1 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvd/Denver-Chico_collaboration/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#BVD read in (work)
lvl1 <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/HLM_LASSO_Dump_S2_S3_lvl1.csv")
lvl2 <- read.csv("/Users/bvandusen/Dropbox/work/Research/LASSO/data/HLM_LASSO_Dump_S2_S3_lvl2.csv")

#Assign row numbers
lvl1$row <- 1:nrow(lvl1)

#Bring lvl2_imp into lvl1_imp
lvl1 <- left_join(lvl1, lvl2[,c("Assessment_Sequence_ID", "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI")], by = "Assessment_Sequence_ID")
#Calculate absolute gain for filtering students later.
lvl1$absgain <- lvl1$POST.score-lvl1$PRE.score

#turn dummy variable in a single factor

lvl1$gender <- factor(lvl1$male*1 + lvl1$female*2 + lvl1$transgender*3 + lvl1$other*4, labels = c("NA", "male", "female", "transgender", "gender_other"))
lvl1$instrument <- factor(lvl1$PCA*1 + lvl1$IMCA*2 + lvl1$GCA*3 + lvl1$CINS*4 + lvl1$CCI*5 + lvl1$FMCE*6 + lvl1$BEMA*7 + lvl1$FCI*8 + lvl1$CSEM*9 + lvl1$LSCI*10, labels = c(NA, "PCA", "IMCA", "GCA", "CINS", "CCI", "FMCE", "BEMA", "FCI", "CSEM", "LSCI"))
lvl1$race <- factor(lvl1$white*1 + lvl1$black*2 + lvl1$asian*3 + lvl1$american_indian*4 + lvl1$hawaiian_or_other_pacific_islander*5 + lvl1$other.1*6, labels = c("NA", "white", "black", "asian", "american_indian", "hawaiian_or_other_pacific_islander", "race_other"))

#white is Dom
lvl1$race_URM <- factor(lvl1$white*1 + lvl1$black*2 + lvl1$asian*2 + lvl1$american_indian*2 + lvl1$hawaiian_or_other_pacific_islander*2 + lvl1$other.1*2, labels = c("NA", "Dom", "NonDom"))

#male is Dom
lvl1$gender_URM <- factor(lvl1$male*1 + lvl1$female*2 + lvl1$transgender*2 + lvl1$other*2, labels = c("NA", "Dom", "NonDom"))


#Convert the student time data to numeric strings. The warning is not important.
lvl1$PRE.Duration..Seconds.<-as.numeric(levels(lvl1$PRE.Duration..Seconds.)[lvl1$PRE.Duration..Seconds.])
lvl1$POST.Duration..Seconds.<-as.numeric(levels(lvl1$POST.Duration..Seconds.)[lvl1$POST.Duration..Seconds.])

#Filter out LA's 
lvl1_filt <- lvl1 %>%
  filter(Student.or.LA == 0) #This also removes the students that provided no other information

# remove the students that did not provide either a pre or a post test  **THis is here to watch what happens it can be deleted after this works.
lvl1_filt1 <- subset(lvl1_filt, is.na(PRE.score)==FALSE | is.na(POST.score)== FALSE)

#create and save an unfiltered data set of matched test data
matched_unfiltered <- subset(lvl1_filt1, is.na(PRE.score)==FALSE & is.na(POST.score)==FALSE)
save(matched_unfiltered,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_unfiltered")

#For students that took less than 300 seconds on the pre or post test 
lvl1_filt1$PRE.score[lvl1_filt1$PRE.Duration..Seconds.<300]<- NA
lvl1_filt1$POST.score[lvl1_filt1$POST.Duration..Seconds.<300]<- NA
#Calculate raw gains for filtered data
lvl1_filt1$absgain <- lvl1_filt1$POST.score - lvl1_filt1$PRE.score

#Calculate the Z scores for the absolute gains by instrument to filter out decreases greater than 2 SD
lvl1_filt1$Zabsgain <- ave(lvl1_filt1$absgain, lvl1_filt1$instrument, FUN=scale)
lvl1_filt1$POST.score[lvl1_filt1$Zabsgain< -2 ]<- NA

#Create and save a matched filtered data set
matched_filtered <- subset(lvl1_filt1, is.na(PRE.score)==FALSE & is.na(POST.score)== FALSE)
save(matched_filtered,file="/Users/kerstin/Documents/LA Postdoc stuff/RData/LASSO/Analysis/matched_filtered")
# remove the students that did not provide either a pre or a post test  
lvl1_filt1 <- subset(lvl1_filt1, is.na(PRE.score)==FALSE | is.na(POST.score) == FALSE)


#######These numbers should add up to the number of observations in lvl1_filt1
#thing <- subset(lvl1_filt1, is.na(PRE.score)==FALSE  & is.na(POST.score)==FALSE)
#thing1 <- subset(lvl1_filt1, is.na(PRE.score)==TRUE  & is.na(POST.score)==FALSE)
#thing2 <- subset(lvl1_filt1, is.na(PRE.score)==FALSE  & is.na(POST.score)==TRUE)

####################GIves the N, SD, and Mean by instrument for absolute gains
#absgain.n<-aggregate(absgain2~instrument, lvl1_filt1,NROW)
#absgain.mean<-aggregate(absgain2~instrument, lvl1_filt1,mean)
#absgain.sd<-aggregate(absgain2~instrument, lvl1_filt1,sd)
#renames the columns so that they can be merged
#colnames(absgain.n) <- c("Assessment_Sequence_ID","N")
#colnames(absgain.mean) <- c("Assessment_Sequence_ID","Mean")
#colnames(absgain.sd) <- c("Assessment_Sequence_ID","SD")
#Combine Data sets
#absgain<-merge(absgain.mean,absgain.n)
#absgain<-merge(absgain,absgain.sd)

####################create outputs for filters
#lvl1$absgain <- lvl1$POST.score-lvl1$PRE.score
#lvl1$badgain<-with(lvl1,{ifelse(absgain<= "-20",1,ifelse(absgain> "-20",0,"na"))})
#lowpre <-subset(lvl1, PRE.Duration..Seconds. <300)
#plot(lowpre$PRE.Duration..Seconds.,lowpre$PRE.score)
#lowpost <-subset(lvl1_filt1, POST.Duration..Seconds. <2000 &lowpost$POST.score !="NA")
#plot(lowpost$POST.Duration..Seconds.,lowpost$POST.score)
#hist(lowpost$POST.Duration..Seconds.)
#badgain<-subset(lvl1, absgain< "-20" & POST.Duration..Seconds.<400)
#table(badgain$Assessment_Sequence_ID)

############################
